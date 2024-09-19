
use std::collections::HashMap;

use serde::{Serialize, Deserialize};
use serde_json::Value;
use uuid::Uuid;

use crate::node_sync::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(PartialEq))]
pub enum FromServerMessage {
    NodeDelta {
        diff: NodeDiff,
        default_values: HashMap<Uuid, Value>,
    },
    Dispatch(Uuid, Value),
    Variable(Uuid, Value),
    Ping {
        last_ping: Option<u128>,
        data: u128,
        tag: u64,
    },
    Close,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(PartialEq))]
pub enum FromClientMessage {
    Dispatch(Uuid, Value),
    Variable(Uuid, Value),
    Pong {
        client_time: u128,
        data: u128,
        tag: u64,
    },
    Close,
}
