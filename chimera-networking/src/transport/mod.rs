use std::{pin::Pin, sync::Arc};

use async_trait::async_trait;
use chimera_core::node::{ActiveNodeSource, Node, PassiveNodeSink};
use chimera_utils::BoxedResult;
use futures::{future::BoxFuture, Sink};
use tokio::sync::broadcast;
use tokio_util::sync::CancellationToken;

#[cfg(feature = "transport-websockets")]
pub mod websocket;

pub type BoxSink<'a, T, E> = Pin<Box<dyn Sink<T, Error = E> + Send + Sync + 'a>>;
pub type Task = BoxFuture<'static, BoxedResult>;

#[macro_export]
macro_rules! _transport_type {
    ($ty:ident) => {
        <Self::Transport as $crate::transport::Transport>::$ty
    };
    ($transport:ident, $ty:ident) => {
        <<$transport>::Transport as $crate::transport::Transport>::$ty
    };
}

pub trait Transport {
    type ClientId;

    type Server: Server;
    type ServerHandle: ServerHandle;
    type ServerSink: PassiveNodeSink;
    type ServerTransport;
    type ServerAction;
    type ServerEvent;
    type ServerAddress;

    type Client: Client;
    type ClientHandle: ClientHandle;
    type ClientSource: ActiveNodeSource;
    type ClientTransport;
    type ClientAction;
    type ClientEvent;
    type ClientAddress;
}

pub trait Server {
    type Transport: Transport;

    fn start() -> (_transport_type!(ServerHandle), Task);
}

#[async_trait]
pub trait ServerHandle {
    type Transport: Transport;

    async fn push_node_tree(&mut self, node: Option<Arc<Node>>) -> BoxedResult;

    async fn close_client(&mut self, id: _transport_type!(ClientId)) -> BoxedResult;
    async fn connect_client(
        &mut self,
        id: _transport_type!(ClientId),
        client: _transport_type!(ServerTransport),
    ) -> BoxedResult;

    async fn send(&self, action: _transport_type!(ServerAction)) -> BoxedResult;
    fn open_event_receiver(&self) -> broadcast::Receiver<_transport_type!(ServerEvent)>;

    fn start_server(
        &self,
        cancel: CancellationToken,
        addr: _transport_type!(ServerAddress),
    ) -> Task;

    fn as_sink(&self) -> (_transport_type!(ServerSink), Task);
}

pub trait Client {
    type Transport: Transport;

    fn start() -> (_transport_type!(ClientHandle), Task);
}

#[async_trait]
pub trait ClientHandle {
    type Transport: Transport;

    async fn connect(&mut self, addr: _transport_type!(ClientAddress)) -> BoxedResult;
    async fn disconnect(&mut self) -> BoxedResult;

    async fn send(&self, action: _transport_type!(ClientAction)) -> BoxedResult;
    fn open_event_receiver(&self) -> broadcast::Receiver<_transport_type!(ClientEvent)>;

    fn as_source(&self) -> (_transport_type!(ClientSource), Task);
}
