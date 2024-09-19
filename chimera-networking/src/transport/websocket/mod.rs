use std::{error::Error, marker::PhantomData, net::SocketAddr};

use chimera_core::prelude::ClientId;

use crate::messages::{FromClientMessage, FromServerMessage};

use self::{server::{WebsocketServer, WebsocketServerHandle, WebsocketServerEvent, WebsocketServerSink}, client::{WebsocketClient, WebsocketClientHandle, WebsocketClientEvent, WebsocketClientSource}};

use super::Transport;

pub use tokio_tungstenite;

pub mod server;
pub mod client;

pub trait MessageEncoding where Self: Default + Clone + Sync + Send + 'static {
    type Error: Error + Sync + Send + 'static;
    
    fn from_network_to_client(data: &[u8]) -> Result<FromClientMessage, Self::Error>;
    fn to_network_from_server(message: FromServerMessage) -> Result<String, Self::Error>;

    fn from_network_to_server(data: &[u8]) -> Result<FromServerMessage, Self::Error>;
    fn to_network_from_client(message: FromClientMessage) -> Result<String, Self::Error>;
}

#[derive(Default, Clone)]
pub struct JsonMessageEncoding;

impl MessageEncoding for JsonMessageEncoding {
    type Error = serde_json::Error;

    fn from_network_to_client(data: &[u8]) -> Result<FromClientMessage, Self::Error> {
        serde_json::from_slice(data)
    }

    fn to_network_from_server(message: FromServerMessage) -> Result<String, Self::Error> {
        serde_json::to_string(&message)
    }

    fn from_network_to_server(data: &[u8]) -> Result<FromServerMessage, Self::Error> {
        serde_json::from_slice(data)
    }

    fn to_network_from_client(message: FromClientMessage) -> Result<String, Self::Error> {
        serde_json::to_string(&message)
    }
}

pub struct WebsocketTransport<E: MessageEncoding>(PhantomData<E>);

pub type ClientAddress = tokio_tungstenite::tungstenite::http::Request<()>;

impl<E: MessageEncoding> Transport for WebsocketTransport<E> {
    type ClientId = ClientId;
    
    type Server = WebsocketServer<E>;
    type ServerHandle = WebsocketServerHandle<E>;
    type ServerSink = WebsocketServerSink;
    type ServerTransport = warp::ws::WebSocket;
    type ServerAction = ();
    type ServerEvent = WebsocketServerEvent;
    type ServerAddress = SocketAddr;

    type Client = WebsocketClient<E>;
    type ClientHandle = WebsocketClientHandle<E>;
    type ClientSource = WebsocketClientSource;
    type ClientTransport = warp::ws::WebSocket;
    type ClientAction = ();
    type ClientEvent = WebsocketClientEvent;
    type ClientAddress = ClientAddress;
}
