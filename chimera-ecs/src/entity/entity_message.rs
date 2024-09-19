
use serde::de::DeserializeOwned;
use thiserror::Error;

use crate::prelude::*;

#[derive(Debug, Error)]
#[error("Entity {receiver} received invalid message with type {message_type:?} from {sender:?}: {reason}")]
pub struct InvalidMessageError {
    receiver: Id,
    message_type: Option<String>,
    sender: MessageSender,
    reason: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum SendMode {
    Immediate,
    QueueIfPaused,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum MessageSender {
    Unknown,
    Entity(Id),
    Dispatch(Uuid)
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Message {
    pub sender: MessageSender,
    pub dest: Id,
    pub next: Option<Id>,
    pub send_mode: SendMode,
    pub payload: Vec<Payload>,
}

impl Message {
    pub fn to(dest: Id) -> Self {
        Self {
            sender: MessageSender::Unknown,
            dest,
            next: None,
            send_mode: SendMode::QueueIfPaused,
            payload: Vec::with_capacity(1),
        }
    }

    pub fn from(mut self, from: MessageSender) -> Self {
        self.sender = from;
        self
    }

    pub fn next(mut self, next: Id) -> Self {
        self.next = Some(next);
        self
    }

    pub fn immediate(mut self) -> Self {
        self.send_mode = SendMode::Immediate;
        self
    }

    pub fn with_local<T: LocalPayload>(mut self, local: T) -> Self {
        self.payload.push(Payload::Local(Box::new(local)));
        self
    }

    pub fn with_remote<T: RemotePayload>(mut self, remote: T) -> Self {
        self.payload.push(Payload::Remote(Box::new(remote)));
        self
    }

    pub fn with_value(mut self, value: Value) -> Self {
        self.payload.push(Payload::Value(value));
        self
    }

    pub fn with_payload(mut self, payload: Payload) -> Self {
        self.payload.push(payload);
        self
    }

    pub fn mutate<R>(self, f: impl FnOnce(Self)->R) -> R {
        f(self)
    }

    pub fn make_invalid_msg_error(&self, reason: impl Into<String>) -> BoxedError {
        Box::new(InvalidMessageError {
            receiver: self.dest.clone(),
            message_type: self.message_type().map(|s| s.into_owned()),
            sender: self.sender.clone(),
            reason: reason.into(),
        })
    }

    pub fn message_type(&self) -> Option<CowStr> {
        self.payload.first().and_then(|p| p.get_message_type())
    }

    pub fn is_message_type(&self, types: &[&str]) -> bool {
        if let Some(t) = self.message_type() {
            types.contains(&t.as_ref())
        } else {
            false
        }
    }

    pub fn try_convert_first<M: DeserializeOwned + 'static>(&mut self) -> BoxedResult<M> {
        self.payload.remove(0)
            .try_convert::<M>()
            .map_err(|e| self.make_invalid_msg_error(e.to_string()))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SendStatus {
    Paused,
    Sent,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Query {
    pub sender: MessageSender,
    pub dest: Id,
    pub payload: Option<Payload>,
}

impl Query {
    pub fn new(dest: Id) -> Self {
        Self {
            sender: MessageSender::Unknown,
            dest,
            payload: None
        }
    }

    pub fn message_type(&self) -> Option<CowStr> {
        self.payload.as_ref().and_then(|p| p.get_message_type())
    }

    pub fn is_message_type(&self, types: &[&str]) -> bool {
        if let Some(t) = self.message_type() {
            types.contains(&t.as_ref())
        } else {
            false
        }
    }

    pub fn from(mut self, from: MessageSender) -> Self {
        self.sender = from;
        self
    }

    pub fn with_local<T: LocalPayload>(mut self, local: T) -> Self {
        self.payload = Some(Payload::Local(Box::new(local)));
        self
    }

    pub fn with_remote<T: RemotePayload>(mut self, remote: T) -> Self {
        self.payload = Some(Payload::Remote(Box::new(remote)));
        self
    }

    pub fn with_value(mut self, value: Value) -> Self {
        self.payload = Some(Payload::Value(value));
        self
    }

    pub fn with_payload(mut self, payload: Payload) -> Self {
        self.payload = Some(payload);
        self
    }

    pub fn mutate<R, F: FnOnce(Self)->R>(self, f: F) -> R {
        f(self)
    }

    pub fn make_invalid_msg_error(&self, reason: impl Into<String>) -> BoxedError {
        Box::new(InvalidMessageError {
            receiver: self.dest.clone(),
            message_type: self.message_type().map(|s| s.into_owned()),
            sender: self.sender.clone(),
            reason: reason.into(),
        }) as BoxedError
    }

    pub fn try_convert<M: DeserializeOwned + 'static>(&mut self) -> BoxedResult<M> {
        self.payload.take().unwrap().try_convert::<M>().map_err(|e| self.make_invalid_msg_error(e.to_string()))
    }
}

#[derive(Clone, Debug)]
pub enum QueryResponse {
    Response(Response),
    Paused
}

impl QueryResponse {
    pub fn unwrap_value(self) -> Response {
        match self {
            QueryResponse::Response(v) => v,
            QueryResponse::Paused => panic!("QueryResponse was Paused"),
        }
    }

    pub fn unpaused_or_else<E>(self, f: impl FnOnce()->E) -> Result<Response, E> {
        match self {
            QueryResponse::Response(v) => Ok(v),
            QueryResponse::Paused => Err(f()),
        }
    }

    pub fn try_convert<V: DeserializeOwned + 'static>(self, pause_error: impl FnOnce()->BoxedError) -> BoxedResult<V> {
        match self {
            QueryResponse::Response(v) => Ok(v.try_convert()?),
            QueryResponse::Paused => Err(pause_error()),
        }
    }
}
