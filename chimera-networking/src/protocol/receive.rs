use std::{
    collections::HashMap,
    sync::{Arc, Weak},
};

use chimera_core::prelude::*;
use derivative::Derivative;
use futures::{
    channel::mpsc::{channel, unbounded, Receiver, Sender, UnboundedReceiver, UnboundedSender},
    stream::BoxStream,
    SinkExt, StreamExt,
};
use serde_json::Value;
use tokio::{sync::broadcast, task::JoinHandle};
use chimera_utils::*;
use uuid::Uuid;

use crate::{
    messages::{FromClientMessage, FromServerMessage},
    node_sync::{NodePatcher, TransportStateFactory},
    protocol::now_unix_epoch_ns,
    transport::BoxSink,
};

#[derive(Debug, Clone)]
pub enum ReceiveProtocolEvent {
    Pushed(Option<Arc<Node>>),
    Pinged { ping_ns: u128 },
}

#[derive(Derivative)]
#[derivative(Debug)]
enum ReceiveProtocolAction {
    Shutdown(),
}

#[derive(Clone, Debug)]
pub struct ReceiveProtocolHandle {
    atx: Sender<ReceiveProtocolAction>,
    event: broadcast::Sender<ReceiveProtocolEvent>,
}

impl ReceiveProtocolHandle {
    pub async fn shutdown(&mut self) -> BoxedResult<()> {
        self.atx.send(ReceiveProtocolAction::Shutdown()).await?;
        Ok(())
    }

    pub fn open_event_receiver(&self) -> broadcast::Receiver<ReceiveProtocolEvent> {
        self.event.subscribe()
    }
}

type DispatchMap = Mutex<HashMap<Uuid, Weak<NetworkDispatch>>>;
type VariableMap = Mutex<HashMap<Uuid, Weak<NetworkVariable>>>;

pub type TX = BoxSink<'static, FromClientMessage, BoxedError>;
pub type RX = BoxStream<'static, FromServerMessage>;

pub struct ReceiveProtocol {
    arx: Receiver<ReceiveProtocolAction>,

    tx: TX,
    rx: RX,

    running: bool,

    forward_rx: UnboundedReceiver<FromClientMessage>,

    event: broadcast::Sender<ReceiveProtocolEvent>,

    patcher: NodePatcher,
    factory: ClientFactory,
}

impl ReceiveProtocol {
    pub fn start(tx: TX, rx: RX) -> (ReceiveProtocolHandle, JoinHandle<BoxedResult<()>>) {
        let (atx, arx) = channel(16);

        let (forward_tx, forward_rx) = unbounded();

        let (event, _) = broadcast::channel(16);

        let s = Self {
            arx,

            tx,
            rx,

            running: true,

            forward_rx,
            event: event.clone(),

            patcher: NodePatcher::new(),
            factory: ClientFactory::new(Arc::new(forward_tx)),
        };

        let task = tokio::spawn(async move {
            s.run().await.inspect_err(|e| {
                tracing::error!(what = "error running ReceiveProtocol", why = %e);
            })
        });

        (ReceiveProtocolHandle { atx, event }, task)
    }

    async fn run(mut self) -> BoxedResult<()> {
        while self.running {
            tokio::select! {
                message = self.rx.next() => {
                    let message = match message {
                        Some(m) => m,
                        None => {
                            tracing::info!(what = "self.rx was closed unexpectedly");
                            break;
                        }
                    };

                    self.handle_server(message).await.map_err_explain("could not handle message")?;
                },
                action = self.arx.next() => {
                    let action = match action {
                        Some(a) => a,
                        None => {
                            tracing::info!(what = "self.arx was closed: shutting down");
                            break;
                        }
                    };

                    self.handle_action(action).await.map_err_explain("could not handle action")?;
                },
                message = self.forward_rx.next() => {
                    let message = match message {
                        Some(m) => m,
                        None => {
                            tracing::info!(what = "self.forward_rx was closed unexpectedly");
                            break;
                        }
                    };

                    self.tx.send(message).await.map_err_explain("could not send message to tx")?;
                },
            };
        }

        Ok(())
    }

    async fn handle_server(&mut self, msg: FromServerMessage) -> BoxedResult<()> {
        match msg {
            FromServerMessage::NodeDelta {
                diff,
                default_values,
            } => {
                let node = self
                    .patcher
                    .push_node_diff(&self.factory, diff, &default_values)?;

                let _ = self.event.send(ReceiveProtocolEvent::Pushed(
                    node.map(|x| Arc::new(x.clone())),
                ));
            }
            FromServerMessage::Dispatch(id, msg) => {
                self.factory.send(&id, msg)?;
            }
            FromServerMessage::Variable(id, value) => {
                self.factory.set(&id, value)?;
            }
            FromServerMessage::Ping {
                last_ping,
                data,
                tag,
            } => {
                let now = now_unix_epoch_ns();

                self.tx
                    .send(FromClientMessage::Pong {
                        client_time: now,
                        data,
                        tag,
                    })
                    .await?;

                if let Some(last_ping) = last_ping {
                    let _ = self
                        .event
                        .send(ReceiveProtocolEvent::Pinged { ping_ns: last_ping });
                }
            }
            FromServerMessage::Close => {
                self.running = false;
            }
        }

        Ok(())
    }

    async fn handle_action(&mut self, action: ReceiveProtocolAction) -> BoxedResult {
        match action {
            ReceiveProtocolAction::Shutdown() => {
                tracing::info!(what = "gracefully closing websocket");
                self.running = false;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
struct ClientFactory {
    tx: Arc<UnboundedSender<FromClientMessage>>,
    dispatches: Arc<DispatchMap>,
    vars: Arc<VariableMap>,
}

impl ClientFactory {
    pub fn new(tx: Arc<UnboundedSender<FromClientMessage>>) -> Self {
        Self {
            tx,
            dispatches: Arc::new(Mutex::new(HashMap::new())),
            vars: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn send(&self, id: &Uuid, msg: Value) -> BoxedResult<()> {
        let d = lock_with_timeout(&self.dispatches)?
            .get(id)
            .ok_or_else(|| "server tried to send message to dispatch that is not bound")?
            .upgrade_or_err(format!("Dispatch {id}").as_str())?;

        unsafe {
            d.send(DispatchMessage {
                message: Payload::Value(msg),
            })?;
        }

        Ok(())
    }

    pub fn set(&self, id: &Uuid, value: Value) -> BoxedResult<()> {
        let d = lock_with_timeout(&self.vars)?
            .get(id)
            .ok_or_else(|| "server tried to update variable that is not bound")?
            .upgrade_or_err(format!("Variable {id}").as_str())?;

        d.handle_changed(value)?;

        Ok(())
    }
}

impl TransportStateFactory for ClientFactory {
    fn get_dispatch(
        &self,
        id: &Uuid,
        allow_rx: bool,
        allow_tx: bool,
    ) -> BoxedResult<Arc<dyn Dispatch>> {
        let _ = allow_rx;

        let mut dispatches = lock_with_timeout(&self.dispatches)?;

        Ok({
            dispatches
                .get(id)
                .and_then(|w| w.upgrade())
                .unwrap_or_else(|| {
                    let disp = Arc::new(NetworkDispatch::new(
                        id.clone(),
                        allow_rx,
                        allow_tx,
                        Arc::downgrade(&self.dispatches),
                        self.tx.clone(),
                    ));

                    dispatches.insert(id.clone(), Arc::downgrade(&disp));

                    disp
                })
        })
    }

    fn get_variable(
        &self,
        id: &Uuid,
        allow_rx: bool,
        allow_tx: bool,
        default: Value,
    ) -> BoxedResult<Arc<dyn Variable>> {
        let mut vars = lock_with_timeout(&self.vars)?;

        Ok({
            vars.get(id).and_then(|w| w.upgrade()).unwrap_or_else(|| {
                let var = Arc::new(NetworkVariable::new(
                    id.clone(),
                    allow_rx,
                    allow_tx,
                    default,
                    self.tx.clone(),
                ));

                vars.insert(id.clone(), Arc::downgrade(&var));

                var
            })
        })
    }
}

#[derive(Debug)]
struct NetworkDispatch {
    id: Uuid,
    allow_rx: bool,
    perms: Arc<DispatchPermissions>,

    tx: BroadcastEvent<DispatchMessage>,
    rx: Delegate<DispatchMessage>,

    dispatches: Weak<Mutex<HashMap<Uuid, Weak<NetworkDispatch>>>>,
}

impl NetworkDispatch {
    pub fn new(
        id: Uuid,
        allow_rx: bool,
        allow_tx: bool,
        dispatches: Weak<DispatchMap>,
        tx: Arc<UnboundedSender<FromClientMessage>>,
    ) -> Self {
        Self {
            id: id.clone(),
            allow_rx,
            perms: Arc::new(DispatchPermissions {
                tx: PermissionSet::allow(allow_tx),
                rx: PermissionSet::allow(allow_rx),
            }),
            tx: BroadcastEvent::new(),
            rx: Delegate::new(Box::new(move |msg| {
                if allow_tx {
                    if msg.message.is_serializable() {
                        let value = msg
                            .message
                            .clone()
                            .into_value()
                            .map_err_explain("could not serialize dispatch message")?;

                        tx.unbounded_send(FromClientMessage::Dispatch(id.clone(), value))?;

                        Ok(())
                    } else {
                        Err(format!("cannot send non-serializable dispatch message across network boundary: {msg:?}").to_boxed_error())
                    }
                } else {
                    Err(
                        format!("cannot send message to dispatch without tx permissions")
                            .to_boxed_error(),
                    )
                }
            })),

            dispatches,
        }
    }
}

impl Drop for NetworkDispatch {
    fn drop(&mut self) {
        if let Some(map) = self.dispatches.upgrade() {
            if let Ok(mut d) = lock_with_timeout(&*map) {
                d.remove(&self.id);
            }
        }
    }
}

impl Dispatch for NetworkDispatch {
    unsafe fn send(&self, msg: DispatchMessage) -> BoxedResult<()> {
        if self.allow_rx {
            self.tx.invoke(&msg)
        } else {
            Err("cannot send to a dispatch without rx permissions".to_boxed_error())
        }
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

    fn get_permissions(&self) -> BoxedResult<Option<Arc<DispatchPermissions>>> {
        Ok(Some(self.perms.clone()))
    }
}

/// Calculates the curve for a given stream of `VariableValue`s
#[allow(dead_code)]
struct VariableStreamCompressor {
    dt: u64,
}

#[derive(Debug)]
struct NetworkVariable {
    id: Uuid,
    allow_rx: bool,
    allow_tx: bool,
    perms: Arc<VariablePermissions>,

    value: RwLock<Value>,
    changed: BroadcastEvent<Value>,

    tx: Arc<UnboundedSender<FromClientMessage>>,
}

impl NetworkVariable {
    pub fn new(
        id: Uuid,
        allow_rx: bool,
        allow_tx: bool,
        default: Value,
        tx: Arc<UnboundedSender<FromClientMessage>>,
    ) -> Self {
        Self {
            id,
            allow_rx,
            allow_tx,
            perms: Arc::new(VariablePermissions {
                read: PermissionSet::allow(allow_rx),
                write: PermissionSet::allow(allow_tx),
            }),
            value: RwLock::new(default),
            changed: BroadcastEvent::new(),
            tx,
        }
    }

    pub fn handle_changed(&self, value: Value) -> BoxedResult<()> {
        *write_lock_with_timeout(&self.value)? = value.clone();
        self.changed.invoke(&value)?;
        Ok(())
    }
}

impl Variable for NetworkVariable {
    fn get(&self) -> BoxedResult<Value> {
        if self.allow_rx {
            Ok(read_lock_with_timeout(&self.value)?.clone())
        } else {
            Err("cannot read write-only variable".to_boxed_error())
        }
    }

    fn set(&self, value: Value) -> BoxedResult<()> {
        if self.allow_tx {
            let changed = self.value.with_mut(|v| {
                if v != &value {
                    *v = value.clone();
                    true
                } else {
                    false
                }
            })?;

            if changed {
                self.changed.invoke(&value)?;
                self.tx
                    .unbounded_send(FromClientMessage::Variable(self.id.clone(), value))?;
            }

            Ok(())
        } else {
            Err("cannot write read-only variable".to_boxed_error())
        }
    }

    fn on_changed(&self) -> SubscriptionWatchRef<Value> {
        &self.changed
    }

    fn get_id(&self) -> Uuid {
        self.id.clone()
    }

    fn get_permissions(&self) -> BoxedResult<Option<Arc<VariablePermissions>>> {
        Ok(Some(self.perms.clone()))
    }
}
