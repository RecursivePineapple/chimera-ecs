
use std::{marker::PhantomData, time::Duration, sync::Arc};

use async_trait::async_trait;
use chimera_core::node::{Node, ActiveNodeSource};
use futures::{channel::mpsc, StreamExt, SinkExt, future::BoxFuture};
use tokio::{net::TcpStream, task::JoinHandle, sync::{broadcast}};
use tracing::info;
use chimera_utils::*;
use tokio_tungstenite::{WebSocketStream, MaybeTlsStream, tungstenite::Message, connect_async};

use super::{MessageEncoding, WebsocketTransport, ClientAddress};

use crate::{messages::{FromServerMessage, FromClientMessage}, protocol::receive::{ReceiveProtocol, ReceiveProtocolEvent, ReceiveProtocolHandle}, transport::{ClientHandle, Client, Task}};

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum WebsocketClientEvent {
    Pushed(Option<Arc<Node>>),
    Pinged { ping_ns: u128 },
    Connected,
    Disconnected,
}

#[derive(Debug)]
pub enum WebsocketClientAction {
    Connect(ClientAddress),
    Disconnect,
}

#[derive(Debug, Clone)]
pub struct WebsocketClientHandle<E: MessageEncoding> {
    atx: mpsc::Sender<WebsocketClientAction>,
    event: broadcast::Sender<WebsocketClientEvent>,
    _e: PhantomData<E>,
}

#[async_trait]
impl<E: MessageEncoding> ClientHandle for WebsocketClientHandle<E> {
    type Transport = WebsocketTransport<E>;

    async fn connect(&mut self, addr: ClientAddress) -> BoxedResult {
        self.atx.send(WebsocketClientAction::Connect(addr)).await?;
        Ok(())
    }

    async fn disconnect(&mut self) -> BoxedResult {
        self.atx.send(WebsocketClientAction::Disconnect).await?;
        Ok(())
    }

    async fn send(&self, _: ()) -> BoxedResult {
        Ok(())
    }

    fn open_event_receiver(&self) -> broadcast::Receiver<WebsocketClientEvent> {
        self.event.subscribe()
    }

    fn as_source(&self) -> (WebsocketClientSource, Task) {

        let push = BroadcastEvent::new();
        let mut evt = self.open_event_receiver();

        (
            WebsocketClientSource(push.clone()),
            Box::pin(async move {
                loop {
                    match evt.recv().await? {
                        WebsocketClientEvent::Pushed(tree) => {
                            push.invoke(&tree)?;
                        }
                        _ => {

                        }
                    }
                }
            })
        )
    }
}

pub struct WebsocketClientSource(BroadcastEvent<Option<Arc<Node>>>);

impl ActiveNodeSource for WebsocketClientSource {
    fn on_push(&self) -> SubscriptionWatchRef<'_, Option<Arc<Node>>> {
        &self.0
    }
}

pub struct WebsocketClient<E: MessageEncoding> {
    arx: mpsc::Receiver<WebsocketClientAction>,

    ws: Option<WebSocketStream<MaybeTlsStream<TcpStream>>>,

    tx: mpsc::Sender<FromServerMessage>,
    rx: mpsc::Receiver<FromClientMessage>,
    proto_event: broadcast::Receiver<ReceiveProtocolEvent>,
    _proto: ReceiveProtocolHandle,
    proto_task: JoinHandle<BoxedResult>,

    event: broadcast::Sender<WebsocketClientEvent>,

    _e: PhantomData<E>,
}

impl<E: MessageEncoding> Client for WebsocketClient<E> {
    type Transport = WebsocketTransport<E>;

    fn start() -> (WebsocketClientHandle<E>, Task) {
        let (atx, arx) = mpsc::channel(16);

        let (event, _) = broadcast::channel(16);

        let (txtx, txrx) = mpsc::channel::<FromServerMessage>(16);
        let (rxtx, rxrx) = mpsc::channel::<FromClientMessage>(16);

        let (proto, proto_task) = ReceiveProtocol::start(
            Box::pin(rxtx.sink_map_err(|e| Box::new(e) as BoxedError)),
            Box::pin(txrx)
        );

        let s = Self {
            arx,

            ws: None,

            tx: txtx,
            rx: rxrx,
            proto_event: proto.open_event_receiver(),
            _proto: proto,
            proto_task,

            event: event.clone(),

            _e: PhantomData
        };

        (
            WebsocketClientHandle { atx, event, _e: Default::default() },
            Box::pin(async move {
                s.run().await
                    .inspect(|_| { tracing::info!(what = "WebsocketTransportClient has shut down."); })
                    .inspect_err(|e| { tracing::error!(what = "error running WebsocketTransportClient", why = %e); })
            })
        )
    }
}

impl<E: MessageEncoding> WebsocketClient<E> {
    async fn run(mut self) -> BoxedResult {
        loop {
            tokio::select! {
                action = self.arx.next() => {
                    let action = match action {
                        Some(a) => a,
                        None => {
                            tracing::info!(what = "self.arx was closed: shutting down");
                            self.disconnect().await?;
                            break;
                        }
                    };

                    self.handle_action(action).await.map_err_explain("could not handle action")?;
                },
                msg = Self::poll_ws(self.ws.as_mut()) => {
                    let msg = match msg {
                        Some(msg) => msg.map_err_explain("received invalid message")?,
                        None => {
                            tracing::info!(what = "self.ws was closed unexpectedly");
                            self.disconnect().await.map_err_explain("could not disconnect")?;
                            continue;
                        }
                    };

                    let msg = if msg.is_close() {
                        tracing::info!(what = "received close websocket message from server");
                        FromServerMessage::Close
                    } else {
                        E::from_network_to_server(&msg.into_data()[..]).map_err_explain("could not deserialize message")?
                    };

                    tracing::debug!(what = "received message", ?msg);

                    self.tx.send(msg).await.map_err_explain("could not send message to ReceiveProtocol")?;
                },
                msg = self.rx.next(), if self.ws.is_some() => {
                    let msg = match msg {
                        Some(msg) => msg,
                        None => {
                            tracing::info!(what = "self.rx was closed unexpectedly");
                            self.disconnect().await.map_err_explain("could not disconnect")?;
                            continue;
                        }
                    };

                    tracing::debug!(what = "sending message", ?msg);

                    let msg = if let FromClientMessage::Close = &msg {
                        tracing::info!(what = "sending close websocket message to server");
                        Message::Close(None)
                    } else {
                        Message::Text(E::to_network_from_client(msg).map_err_explain("could not serialize message")?)
                    };

                    self.ws.as_mut().unwrap().send(msg).await.map_err_explain("could not send message to websocket")?;
                },
                event = self.proto_event.recv() => {
                    let event = event.map_err_explain("error receiving event from ReceiverProtocol")?;

                    self.handle_event(event).await.map_err_explain("could not handle protocol event")?;
                },
                err = &mut self.proto_task => {
                    err.map_err(|e| BoxedError::from(e)).flatten().map_err_explain("error while running ReceiverProtocol")?;
                    break;
                }
            }
        }

        Ok(())
    }

    fn poll_ws(ws: Option<&mut WebSocketStream<MaybeTlsStream<TcpStream>>>) -> BoxFuture<Option<Result<Message, tokio_tungstenite::tungstenite::Error>>> {
        match ws {
            Some(ws) => Box::pin(ws.next()),
            None => Box::pin(futures::future::pending()),
        }
    }

    async fn handle_action(&mut self, action: WebsocketClientAction) -> BoxedResult {
        match action {
            WebsocketClientAction::Connect(url) => {
                self.disconnect().await.map_err_explain("could not disconnect")?;
                self.connect(url).await.map_err_explain("could not connect")?;
            },
            WebsocketClientAction::Disconnect => {
                self.disconnect().await.map_err_explain("could not disconnect")?;
            },
        }

        Ok(())
    }

    async fn handle_event(&mut self, event: ReceiveProtocolEvent) -> BoxedResult {
        match event {
            ReceiveProtocolEvent::Pushed(nodes) => {
                let _ = self.event.send(WebsocketClientEvent::Pushed(nodes));
            },
            ReceiveProtocolEvent::Pinged { ping_ns } => {
                let _ = self.event.send(WebsocketClientEvent::Pinged { ping_ns });
            }
        }

        Ok(())
    }

    async fn connect(&mut self, req: tokio_tungstenite::tungstenite::http::Request<()>) -> BoxedResult {
        info!(what = "client transport connecting to url", who = ?req);

        let (ws, _) = connect_async(req).await
            .map_err_explain("failed to connect to server")?;

        self.ws = Some(ws);

        self.event.send(WebsocketClientEvent::Connected)?;

        Ok(())
    }

    async fn disconnect(&mut self) -> BoxedResult {
        if let Some(ws) = self.ws.as_mut() {
            info!(what = "client transport is attempting to disconnect from server");

            if let Err(e) = tokio::time::timeout(Duration::from_secs(1), ws.send(Message::Close(None))).await {
                tracing::error!(what = "failed to disconnect", why = %e);
            }

            self.event.send(WebsocketClientEvent::Disconnected)?;

            self.ws = None;
        }

        Ok(())
    }
}
