use std::{collections::HashMap, fmt::Debug, sync::Arc, time::{Instant, Duration}, hash::Hasher};

use chimera_core::prelude::*;

use futures::{stream::BoxStream, channel::mpsc::{Receiver, channel, Sender, UnboundedSender, UnboundedReceiver, unbounded}, SinkExt, TryStreamExt, StreamExt};
use getrandom::getrandom;
use rs_hmac::Hmac;
use rs_sha3_512::Sha3_512State;
use serde_json::Value;
use tokio::{task::JoinHandle, sync::broadcast};
use chimera_utils::*;
use uuid::Uuid;

use crate::{node_sync::*, messages::*, transport::{BoxSink, websocket::server::WebsocketServerEvent}, protocol::now_unix_epoch_ns};

#[derive(Debug)]
enum TransmitProtocolAction {
    Push(Option<Arc<Node>>),
    SetId(ClientId),
    SetFragments(Vec<ClientId>),
    Shutdown(),
}

#[derive(Debug, Clone)]
#[must_use = "clients are closed when this handle is dropped"]
pub struct TransmitProtocolHandle {
    atx: Sender<TransmitProtocolAction>
}

impl TransmitProtocolHandle {
    pub async fn push_node_tree(&mut self, tree: Option<Arc<Node>>) -> BoxedResult<()> {
        self.atx.send(TransmitProtocolAction::Push(tree)).await?;
        Ok(())
    }

    pub async fn set_id(&mut self, id: ClientId) -> BoxedResult<()> {
        self.atx.send(TransmitProtocolAction::SetId(id)).await?;
        Ok(())
    }

    pub async fn set_fragments(&mut self, fragments: Vec<ClientId>) -> BoxedResult<()> {
        self.atx.send(TransmitProtocolAction::SetFragments(fragments)).await?;
        Ok(())
    }

    /// Immediately kills the connection.
    pub async fn shutdown(&mut self) {
        let _ = self.atx.send(TransmitProtocolAction::Shutdown()).await;
    }
}

pub struct TransmitProtocol {
    arx: Receiver<TransmitProtocolAction>,

    tx: BoxSink<'static, FromServerMessage, BoxedError>,
    rx: BoxStream<'static, BoxedResult<FromClientMessage>>,
    event: broadcast::Sender<WebsocketServerEvent>,

    running: bool,

    forward_tx: UnboundedSender<FromServerMessage>,
    forward_rx: UnboundedReceiver<FromServerMessage>,

    differ: NodeDiffer,
    watched_vars: HashMap<Uuid, (u32, Option<Subscription<Value>>, Option<Arc<dyn Variable>>)>,
    bound_dispatches: HashMap<Uuid, (u32, Arc<NetworkDispatch>, DispatchConnection)>,

    id: ClientId,
    fragments: Vec<ClientId>,

    tree: Option<Arc<Node>>,

    key: Vec<u8>,
    last_ping: Option<u128>,
    client_time_offset: Option<i128>,
}

impl TransmitProtocol {
    pub fn start(
        id: ClientId,
        tx: BoxSink<'static, FromServerMessage, BoxedError>,
        rx: BoxStream<'static, BoxedResult<FromClientMessage>>,
        event: broadcast::Sender<WebsocketServerEvent>
    ) -> BoxedResult<(TransmitProtocolHandle, JoinHandle<BoxedResult<()>>)> {
        let (atx, arx) = channel(16);

        let (forward_tx, forward_rx) = unbounded();

        let mut key = vec![0u8; 64];

        for i in 0..10 {
            if let Err(e) = getrandom(&mut key[..]) {
                if i == 9 {
                    return Err(e).map_err_with_backtrace();
                } else {
                    key.fill(0);
                    std::thread::yield_now();
                }
            }
        }

        let mut differ = NodeDiffer::new();

        differ.ident = vec![id.clone()];

        let s = Self {
            arx,

            tx,
            rx,
            event,

            running: true,

            forward_tx,
            forward_rx,

            differ,
            watched_vars: HashMap::new(),
            bound_dispatches: HashMap::new(),

            id,
            fragments: Vec::new(),

            tree: None,

            key,
            last_ping: None,
            client_time_offset: None,
        };

        let task = tokio::spawn(async move {
            s.run().await.inspect_err(|e| {
                tracing::error!(what = "error running TransmitProtocol", why = %e);
            })
        });

        Ok((TransmitProtocolHandle { atx }, task))
    }
    
    async fn run(mut self) -> BoxedResult<()> {
        let ping_period = Duration::from_secs_f32(1.0);

        let mut next_ping = Instant::now();

        while self.running {
            tokio::select! {
                action = self.arx.next() => {
                    let action = match action {
                        Some(a) => a,
                        None => break
                    };

                    self.handle_action(action).await.map_err_explain("could not handle action")?;
                },
                msg = self.rx.try_next() => {
                    let msg = match msg? {
                        Some(m) => m,
                        None => break
                    };

                    self.handle_client(msg).await.map_err_explain("could not handle client message")?;
                },
                msg = self.forward_rx.next() => {
                    let msg = match msg {
                        Some(m) => m,
                        None => break
                    };

                    self.tx.send(msg).await.map_err_explain("could not send server message to transport")?;
                },
                _ = tokio::time::sleep_until(next_ping.into()) => {
                    next_ping = Instant::now() + ping_period;

                    let now = now_unix_epoch_ns();
                    
                    let mut hmac_hash = Hmac::<Sha3_512State, 64>::new(&self.key[..]);
                    hmac_hash.write_u128(now);
                    let tag = hmac_hash.finish();

                    self.tx.send(FromServerMessage::Ping {
                        last_ping: self.last_ping.clone(),
                        data: now,
                        tag,
                    }).await.map_err_explain("could not send ping message to transport")?;
                }
            }
        }

        Ok(())
    }

    async fn handle_action(&mut self, action: TransmitProtocolAction) -> BoxedResult {
        let mut ids_changed = false;

        match action {
            TransmitProtocolAction::Push(tree) => {
                self.tree = tree.clone();
                if let Some(message) = self.push_node_tree(tree)? {
                    self.tx.send(message).await?;
                }
            }
            TransmitProtocolAction::SetId(id) => {
                self.id = id;
                ids_changed = true;
            }
            TransmitProtocolAction::SetFragments(frags) => {
                self.fragments = frags;
                ids_changed = true;
            }
            TransmitProtocolAction::Shutdown() => {
                tracing::info!(what = "shutdown requested for client", who = ?self.id);
                match tokio::time::timeout(Duration::from_secs_f32(5.0), self.tx.send(FromServerMessage::Close)).await {
                    Ok(Ok(_)) => {
                        tracing::debug!(what = "sent close to transport", who = ?self.id);
                    },
                    Ok(Err(e)) => {
                        tracing::error!(what = "could not send close to transport", why = e, who = ?self.id);
                    },
                    Err(_) => {
                        tracing::error!(what = "could not send close to transport: timed out", who = ?self.id);
                    },
                }
                self.running = false;
            }
        }

        if ids_changed {
            self.differ.ident = self.fragments.clone();
            self.differ.ident.push(self.id.clone());
            self.differ.ident.sort();

            if let Some(message) = self.push_node_tree(self.tree.clone())? {
                self.tx.send(message).await?;
            }
        }

        Ok(())
    }

    async fn handle_client(&mut self, msg: FromClientMessage) -> BoxedResult<()> {
        match msg {
            FromClientMessage::Dispatch(id, msg) => {
                unsafe {
                    self.bound_dispatches
                        .get(&id)
                        .ok_or_else(|| {
                            format!("client attempted to send message to invalid dispatch {id}")
                                .to_boxed_error()
                        })?
                        .1
                        .send(DispatchMessage { message: Payload::Value(msg) })?;
                }
            }
            FromClientMessage::Variable(id, value) => {
                self.watched_vars.get(&id).as_ref()
                    .ok_or_else(|| format!("client attempted to update invalid variable {id}").to_boxed_error())?
                    .2.as_ref()
                    .ok_or_else(|| format!("client attempted to update variable it didn't have write permissions for: {id}").to_boxed_error())?
                    .set(value)?;
            }
            FromClientMessage::Pong { client_time, data: then, tag } => {
                let now = now_unix_epoch_ns();

                let mut hmac_hash = Hmac::<Sha3_512State, 64>::new(&self.key[..]);
                hmac_hash.write_u128(then);
                let expected_tag = hmac_hash.finish();

                if tag != expected_tag {
                    return Err(format!(
                        "client {:?} sent an invalid pong tag (was {tag}, expected {expected_tag})",
                        &self.differ.ident,
                    ).to_boxed_error());
                }

                let rtt: u128 = now - then;
                let mut ping_ns = rtt / 2; // probably good enough

                if let Some(last_ping) = &mut self.last_ping {
                    ping_ns = *last_ping * 9 / 10 + ping_ns / 10;
                    *last_ping = ping_ns;
                } else {
                    self.last_ping = Some(ping_ns);
                }

                let theo_server_at_pong = then + ping_ns;
                let mut time_offset = (client_time as i128) - (theo_server_at_pong as i128);

                if let Some(client_time_offset) = &mut self.client_time_offset {
                    time_offset = *client_time_offset * 9 / 10 + time_offset / 10;
                    *client_time_offset = time_offset;
                } else {
                    self.client_time_offset = Some(time_offset);
                }

                tracing::debug!(what = "received pong", who = ?self.differ.ident, rtt_ms = ?(rtt as f32 / 1_000_000.0), offset_ms = ?(time_offset as f32 / 1_000_000.0));

                let _ = self.event.send(WebsocketServerEvent::Pinged {
                    who: self.id.clone(),
                    ping_ns,
                    time_offset
                });
            },
            FromClientMessage::Close => {
                self.running = false;
            }
        }

        Ok(())
    }

    fn push_node_tree(&mut self, node: Option<Arc<Node>>) -> BoxedResult<Option<FromServerMessage>> {
        let (diff, subs) = self
            .differ
            .push_node_tree(node.as_ref().map(Arc::as_ref))
            .map_err_explain("could not diff node tree")?;

        if diff != NodeDiff::Same || subs.len() > 0 {
            let mut values = HashMap::new();

            for sub in subs {
                match &sub {
                    SubDiff::ConnectVariable { v, rx, tx: _ } if *rx => {
                        values.insert(
                            v.get_id().clone(),
                            v.get().map_err_explain("could not fetch variable value")?,
                        );
                    }
                    _ => {}
                }

                self.sync_sub(sub)
                    .map_err_explain("could not sync subscriptions")?;
            }

            Ok(Some(FromServerMessage::NodeDelta {
                diff,
                default_values: values,
            }))
        } else {
            Ok(None)
        }
    }

    fn sync_sub(&mut self, sub: SubDiff) -> BoxedResult<()> {
        match sub {
            SubDiff::ConnectDispatch { d, rx, tx } => {
                let id = d.get_id()?;
                if let Some(nd) = self.bound_dispatches.get_mut(&id) {
                    nd.0 += 1;
                } else {
                    let nd = Arc::new(NetworkDispatch::new(id.clone(), self.forward_tx.clone()));

                    let conn = DispatchConnection::connect(&d, &nd, rx, tx)?;

                    self.bound_dispatches.insert(id.clone(), (1, nd, conn));
                }
            }
            SubDiff::DisconnectDispatch(d) => {
                let id = d.get_id()?;
                let nd = self.bound_dispatches.get_mut(&id)
                    .ok_or_else(|| format!("attempted to disconnect dispatch {id} when it wasn't connected").to_boxed_error())?;

                nd.0 -= 1;

                if nd.0 <= 0 {
                    let _ = nd;
                    let _ = self.bound_dispatches.remove(&id);
                }
            }
            SubDiff::ConnectVariable { v, rx, tx } => {
                let id = v.get_id();
                if let Some(nv) = self.watched_vars.get_mut(&id) {
                    nv.0 += 1;
                } else {
                    let rx = rx
                        .then(|| {
                            v.on_changed().watch({
                                let tx = self.forward_tx.clone();
                                let cache = LocalVariable::new("cache".to_string(), v.get()?);

                                Box::new(move |value| {
                                    if value != &cache.get()? {
                                        cache.set(value.clone())?;
                                        let _ = tx.unbounded_send(FromServerMessage::Variable(id.clone(), value.clone()));
                                    }

                                    Ok(())
                                })
                            })
                        })
                        .inside_out()?;

                    let tx = tx.then(|| v.clone());

                    self.watched_vars.insert(id.clone(), (1, rx, tx));
                }
            }
            SubDiff::DisconnectVariable(v) => {
                let id = v.get_id();
                let nv = self.watched_vars.get_mut(&id)
                    .ok_or_else(|| format!("attempted to disconnect variable {id} when it wasn't connected").to_boxed_error())?;

                nv.0 -= 1;

                if nv.0 <= 0 {
                    let _ = nv;
                    let _ = self.watched_vars.remove(&id)
                        .ok_or("expected watched_vars to contain variable since it was just retrieved".to_boxed_error())?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
struct NetworkDispatch {
    id: Uuid,
    rx: Delegate<DispatchMessage>,
    tx: BroadcastEvent<DispatchMessage>,
}

impl NetworkDispatch {
    pub fn new(id: Uuid, tx: UnboundedSender<FromServerMessage>) -> Self {
        Self {
            id: id.clone(),
            rx: Delegate::new(Box::new(move |value| {
                if value.message.is_serializable() {
                    let value = value.message.clone().into_value()
                        .map_err_explain("could not serialize dispatch message")?;

                    let _ = tx.unbounded_send(FromServerMessage::Dispatch(id.clone(), value));

                    Ok(())
                } else {
                    Err(format!("cannot send non-serializable dispatch message across network boundary: {value:?}").to_boxed_error())
                }
            })),
            tx: BroadcastEvent::new(),
        }
    }
}

impl Dispatch for NetworkDispatch {
    unsafe fn send(&self, value: DispatchMessage) -> BoxedResult<()> {
        self.tx.invoke(&value)
    }

    fn get_rx(&self) -> DynInvokeRef<DispatchMessage> {
        &self.rx
    }

    fn get_tx(&self) -> SubscriptionWatchRef<DispatchMessage> {
        &self.tx
    }

    fn get_id(&self) -> BoxedResult<Uuid> {
        Ok(self.id.clone())
    }
}
