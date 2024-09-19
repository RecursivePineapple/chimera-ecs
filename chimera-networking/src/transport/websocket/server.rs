use std::{
    collections::HashMap,
    marker::PhantomData,
    net::SocketAddr,
    pin::Pin,
    sync::Arc,
    task::Poll,
    time::{Duration, Instant},
};

use async_trait::async_trait;
use chimera_core::{
    node::{Node, PassiveNodeSink},
    prelude::ClientId,
};
use futures::{
    channel::mpsc::{channel, Receiver, Sender},
    future::{self, BoxFuture},
    stream::{FuturesUnordered, SplitStream},
    FutureExt, SinkExt, Stream, StreamExt,
};

use tokio::sync::{
    broadcast,
    mpsc::{unbounded_channel, UnboundedSender},
};
use tokio_util::sync::CancellationToken;
use tracing::{error, info};
use chimera_utils::*;
use uuid::Uuid;
use warp::{
    ws::{Message as WsMessage, WebSocket},
    Filter, Rejection, Reply,
};

use super::{MessageEncoding, WebsocketTransport};

use crate::{
    messages::*,
    protocol::transmit::{TransmitProtocol, TransmitProtocolHandle},
    transport::{Server, ServerHandle, Task},
};

#[derive(Debug)]
enum WebsocketServerAction {
    Push(Option<Arc<Node>>),
    Connect(ClientId, WebSocket),
    Disconnect(ClientId),
}

#[derive(Debug, Clone)]
pub enum WebsocketServerEvent {
    Connected(ClientId, TransmitProtocolHandle),
    Disconnected(ClientId),
    Pinged {
        who: ClientId,
        ping_ns: u128,
        time_offset: i128,
    },
}

#[derive(Debug, Clone)]
pub struct WebsocketServerHandle<E: MessageEncoding> {
    atx: Sender<WebsocketServerAction>,
    event: broadcast::Sender<WebsocketServerEvent>,
    _e: PhantomData<E>,
}

#[async_trait]
impl<E: MessageEncoding> ServerHandle for WebsocketServerHandle<E> {
    type Transport = WebsocketTransport<E>;

    async fn push_node_tree(&mut self, node: Option<Arc<Node>>) -> BoxedResult<()> {
        self.atx.send(WebsocketServerAction::Push(node)).await?;
        Ok(())
    }

    async fn close_client(&mut self, id: ClientId) -> BoxedResult<()> {
        self.atx.send(WebsocketServerAction::Disconnect(id)).await?;
        Ok(())
    }

    async fn connect_client(
        &mut self,
        id: ClientId,
        client: warp::ws::WebSocket,
    ) -> BoxedResult<()> {
        self.atx
            .send(WebsocketServerAction::Connect(id, client))
            .await?;
        Ok(())
    }

    async fn send(&self, _: ()) -> BoxedResult<()> {
        Ok(())
    }

    fn open_event_receiver(&self) -> broadcast::Receiver<WebsocketServerEvent> {
        self.event.subscribe()
    }

    fn start_server(&self, cancel: CancellationToken, addr: SocketAddr) -> Task {
        let connect_client = warp::path!("client" / Uuid)
            .and(warp::ws())
            .and(warp::any().map({
                let handle = self.clone();

                move || handle.clone()
            }))
            .and_then(|id, ws, handle| Self::client_ws_filter(ClientId::Id(id), ws, handle))
            .boxed();

        let connect_singleton = warp::path!("singleton" / String)
            .and(warp::ws())
            .and(warp::any().map({
                let handle = self.clone();

                move || handle.clone()
            }))
            .and_then(|singleton, ws, handle| {
                Self::client_ws_filter(ClientId::Singleton(singleton), ws, handle)
            })
            .boxed();

        let route = connect_client.or(connect_singleton);

        let (_, server) = warp::serve(route).bind_with_graceful_shutdown(addr, async move {
            cancel.cancelled().await;
        });

        Box::pin(server.map(|_| Ok(())))
    }

    fn as_sink(&self) -> (WebsocketServerSink, Task) {
        let (tx, mut rx) = unbounded_channel();

        let mut this = self.clone();

        (
            WebsocketServerSink(tx),
            Box::pin(async move {
                loop {
                    let tree = match rx.recv().await {
                        Some(tree) => tree,
                        None => {
                            break;
                        }
                    };

                    this.push_node_tree(tree).await?;
                }

                Ok(())
            }),
        )
    }
}

pub struct WebsocketServerSink(UnboundedSender<Option<Arc<Node>>>);

impl PassiveNodeSink for WebsocketServerSink {
    fn push(&self, tree: Option<Arc<Node>>) -> BoxedResult<()> {
        self.0.send(tree)?;
        Ok(())
    }
}

impl<E: MessageEncoding> WebsocketServerHandle<E> {
    async fn client_ws_filter(
        id: ClientId,
        ws: warp::ws::Ws,
        handle: WebsocketServerHandle<E>,
    ) -> Result<impl Reply, Rejection> {
        Ok(ws.on_upgrade(move |socket| Self::client_connection(socket, id, handle)))
    }

    async fn client_connection(ws: WebSocket, id: ClientId, mut handle: WebsocketServerHandle<E>) {
        if let Err(e) = handle.connect_client(id, ws).await {
            error!(what = "error sending WebsocketTransportAction::Connect", why = %e);
        }
    }
}

pub struct WebsocketServer<E: MessageEncoding> {
    stx: Sender<WebsocketServerAction>,
    srx: Receiver<WebsocketServerAction>,

    arx: Receiver<WebsocketServerAction>,

    last_tree: Option<Arc<Node>>,

    clients: HashMap<ClientId, TransmitProtocolHandle>,
    client_tasks: FuturesUnordered<BoxFuture<'static, (BoxedResult<()>, ClientId)>>,

    event: broadcast::Sender<WebsocketServerEvent>,

    _e: PhantomData<E>,
}

impl<E: MessageEncoding> Server for WebsocketServer<E> {
    type Transport = WebsocketTransport<E>;

    fn start() -> (WebsocketServerHandle<E>, Task) {
        let (atx, arx) = channel(16);
        let (stx, srx) = channel(16);

        let (event, _) = broadcast::channel(16);

        let s = Self {
            stx,
            srx,
            arx,
            last_tree: None,
            clients: HashMap::new(),
            client_tasks: FuturesUnordered::new(),
            event: event.clone(),
            _e: Default::default(),
        };

        (
            WebsocketServerHandle {
                atx,
                event,
                _e: Default::default(),
            },
            Box::pin(async move {
                s.run().await.inspect_err(|e| {
                    tracing::error!(what = "error running WebsocketTransportServer", why = %e);
                })
            }),
        )
    }
}

impl<E: MessageEncoding> WebsocketServer<E> {
    async fn run(mut self) -> BoxedResult<()> {
        loop {
            tokio::select! {
                action = self.arx.next() => {
                    let action = match action {
                        Some(a) => a,
                        None => {
                            break;
                        }
                    };

                    self.handle_action(action).await.map_err_explain("could not handle action")?;
                },
                closed = self.client_tasks.next(), if !self.client_tasks.is_empty() => {
                    if let Some((status, id)) = closed {
                        match status {
                            Ok(_) => tracing::info!(what = "transmitter has exited without errors", who = ?id),
                            Err(e) => tracing::error!(what = "transmitter has exited with errors", who = ?id, why = %e),
                        }
                        self.clients.remove(&id);
                        let _ = self.event.send(WebsocketServerEvent::Disconnected(id));
                    }
                },
                action = self.srx.next() => {
                    let action = action.expect("stx should never close");

                    self.handle_action(action).await.map_err_explain("could not handle action")?;
                },
            }
        }

        Ok(())
    }

    async fn handle_action(&mut self, action: WebsocketServerAction) -> BoxedResult<()> {
        match action {
            WebsocketServerAction::Push(node) => {
                info!(what = "pushing new node tree");
                self.last_tree = node.clone();

                let mut fut = self
                    .clients
                    .iter_mut()
                    .map(|(k, h)| {
                        let node = node.clone();
                        async move { h.push_node_tree(node).await.map_err(|e| (k.clone(), e)) }
                    })
                    .collect::<FuturesUnordered<_>>();

                let mut dc = Vec::new();

                while let Some(res) = fut.next().await {
                    if let Err((id, e)) = res {
                        error!(what = "error pushing node tree to client", why = %e, who = ?id);
                        dc.push(id);
                    }
                }

                drop(fut);

                for id in dc {
                    self.handle_disconnect(&id).await?;
                }
            }
            WebsocketServerAction::Connect(id, ws) => {
                info!(what = "received new websocket connection", who = ?id);
                self.handle_connect(id, ws).await?;
            }
            WebsocketServerAction::Disconnect(id) => {
                info!(what = "killing websocket connection", who = ?id);
                self.handle_disconnect(&id).await?;
            }
        }

        Ok(())
    }

    async fn handle_connect(&mut self, id: ClientId, ws: WebSocket) -> BoxedResult<()> {
        if let Some(old) = self.clients.get_mut(&id) {
            tracing::warn!(what = "a client connected with a conflicting id: the prior client is being disconnected", who = ?id);

            old.shutdown().await;

            let mut stx = self.stx.clone();
            let mut event = self.event.subscribe();

            tokio::spawn(async move {
                let timeout = Instant::now() + Duration::from_secs(5);

                tokio::select! {
                    _ = tokio::time::sleep_until(timeout.clone().into()) => {
                        tracing::warn!(what = "connection timed out waiting for old client to die");
                    },
                    e = event.recv() => {

                        let e = match e {
                            Ok(e) => e,
                            Err(_) => {
                                return;
                            }
                        };

                        match e {
                            WebsocketServerEvent::Disconnected(dc_id) if dc_id == id => {
                                if let Err(e) = stx.send(WebsocketServerAction::Connect(id.clone(), ws)).await {
                                    tracing::error!(what = "old client disconnected, but could not connect new client", why = ?e, who = ?id);
                                }
                            },
                            _ => {

                            }
                        }
                    }
                }
            });

            return Ok(());
        }

        let (tx, rx) = ws.split();

        let tx = tx.sink_map_err(|e| Box::new(e) as BoxedError).with({
            let id = id.clone();
            move |msg: FromServerMessage| {
                tracing::debug!(what = "sending message", who = ?id, ?msg);
                if let FromServerMessage::Close = &msg {
                    tracing::info!(what = "sending close websocket message to client", who = ?id);
                    future::ready(Ok(WsMessage::close()))
                } else {
                    future::ready(
                        E::to_network_from_server(msg)
                            .map(WsMessage::text)
                            .map_err_boxed(),
                    )
                }
            }
        });

        let (mut handle, task) = TransmitProtocol::start(
            id.clone(),
            Box::pin(tx),
            Box::pin(RxWrapper::<E> {
                rx,
                id: id.clone(),
                closed: false,
                _e: Default::default(),
            }),
            self.event.clone(),
        )?;

        if let Err(e) = handle.push_node_tree(self.last_tree.clone()).await {
            error!(what = "error pushing last node tree to new client", why = %e, who = ?id);
            handle.shutdown().await;
            return Ok(());
        }

        self.clients.insert(id.clone(), handle.clone());
        self.client_tasks.push({
            let id = id.clone();
            Box::pin(async move { (task.await.map_err_boxed().flatten(), id) })
        });

        info!(what = "websocket client connected successfully", who = ?id);

        let _ = self.event.send(WebsocketServerEvent::Connected(id, handle));

        Ok(())
    }

    async fn handle_disconnect(&mut self, id: &ClientId) -> BoxedResult<()> {
        if let Some(c) = self.clients.get_mut(&id) {
            c.shutdown().await;
        }
        let _ = self
            .event
            .send(WebsocketServerEvent::Disconnected(id.clone()));
        Ok(())
    }
}

struct RxWrapper<E: MessageEncoding> {
    rx: SplitStream<WebSocket>,
    id: ClientId,
    closed: bool,
    _e: PhantomData<E>,
}

impl<E: MessageEncoding> Unpin for RxWrapper<E> {}

impl<E: MessageEncoding> Stream for RxWrapper<E> {
    type Item = BoxedResult<FromClientMessage>;

    fn poll_next(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        loop {
            if self.closed {
                return Poll::Ready(None);
            }

            let ws = match self.rx.poll_next_unpin(cx) {
                Poll::Ready(ws) => match ws {
                    Some(Ok(msg)) => msg,
                    Some(Err(e)) => {
                        return Poll::Ready(Some(Err(Box::new(e) as BoxedError)));
                    }
                    None => {
                        self.closed = true;
                        return Poll::Ready(Some(Ok(FromClientMessage::Close)));
                    }
                },
                Poll::Pending => {
                    return Poll::Pending;
                }
            };

            tracing::debug!(what = "received message", who = ?self.id, ?ws);

            if ws.is_close() {
                self.closed = true;
                tracing::info!(what = "received close websocket message from client", who = ?self.id);
                return Poll::Ready(Some(Ok(FromClientMessage::Close)));
            } else if ws.is_binary() || ws.is_text() {
                return Poll::Ready(Some(
                    E::from_network_to_client(ws.as_bytes()).map_err_boxed(),
                ));
            }
        }
    }
}
