use std::{
    any::{type_name, Any},
    fmt::Debug,
};

use chimera_utils::*;
use dyn_clone::{clone_box, DynClone};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_json::Value;

fn get_message_type(v: &Value) -> Option<CowStr> {
    match v {
        Value::Object(o) => o.keys().next().cloned().map(CowStr::Owned),
        _ => None,
    }
}

pub trait LocalPayload: Any + DynClone + Debug + Send + Sync + 'static {
    fn get_message_type(&self) -> Option<CowStr>;
}

pub trait RemotePayload: Any + DynClone + Debug + Send + Sync + 'static {
    fn is_serializable(&self) -> bool;

    fn serialize(&self) -> BoxedResult<Value>;

    fn get_message_type(&self) -> Option<CowStr>;
}

#[derive(Debug)]
pub enum Payload {
    Local(Box<dyn LocalPayload>),
    Remote(Box<dyn RemotePayload>),
    Value(Value),
}

impl Payload {
    pub fn is_serializable(&self) -> bool {
        match self {
            Payload::Local(_) => false,
            Payload::Remote(r) => r.is_serializable(),
            Payload::Value(_) => true,
        }
    }

    pub fn get_message_type(&self) -> Option<CowStr> {
        match self {
            Payload::Local(l) => l.get_message_type(),
            Payload::Remote(r) => r.get_message_type(),
            Payload::Value(v) => get_message_type(v),
        }
    }

    pub fn into_value(&self) -> BoxedResult<Value> {
        match self {
            Payload::Local(l) => Err(format!(
                "cannot convert local payload to json value (payload = {:?})",
                l
            )
            .to_boxed_error()),
            Payload::Remote(r) => r.serialize(),
            Payload::Value(v) => Ok(v.clone()),
        }
    }

    pub fn try_convert<T: DeserializeOwned + 'static>(self) -> BoxedResult<T> {
        match self {
            Self::Local(l) => {
                if (l.as_ref() as &dyn Any).is::<T>() {
                    match (l as Box<dyn Any>).downcast::<T>() {
                        Ok(t) => Ok(*t),
                        Err(_) => panic!(
                            "type ids were the same for type {}: should be downcastable",
                            type_name::<T>()
                        ),
                    }
                } else {
                    Err(format!(
                        "cannot convert local payload to {} (payload = {:?})",
                        type_name::<T>(),
                        l
                    )
                    .to_boxed_error())
                }
            }
            Self::Remote(r) => {
                if (r.as_ref() as &dyn Any).is::<T>() {
                    match (r as Box<dyn Any>).downcast::<T>() {
                        Ok(t) => Ok(*t),
                        Err(_) => panic!(
                            "type ids were the same for type {}: should be downcastable",
                            type_name::<T>()
                        ),
                    }
                } else {
                    let v = r.serialize()?;

                    Ok(serde_json::from_value(v)?)
                }
            }
            Self::Value(v) => Ok(serde_json::from_value(v)?),
        }
    }
}

impl Serialize for Payload {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Payload::Local(l) => Err(serde::ser::Error::custom(format!(
                "cannot serialize local payload {l:?}"
            ))),
            Payload::Remote(r) => {
                let v = r.serialize().map_err(serde::ser::Error::custom)?;

                Serialize::serialize(&v, serializer)
            }
            Payload::Value(v) => Serialize::serialize(&v, serializer),
        }
    }
}

impl<'de> Deserialize<'de> for Payload {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::Value(Value::deserialize(deserializer)?))
    }
}

impl Clone for Payload {
    fn clone(&self) -> Self {
        match self {
            Self::Local(l) => Self::Local(clone_box(l.as_ref())),
            Self::Remote(r) => Self::Remote(clone_box(r.as_ref())),
            Self::Value(v) => Self::Value(v.clone()),
        }
    }
}

impl Default for Payload {
    fn default() -> Self {
        Self::Value(Value::Null)
    }
}

impl PartialEq for Payload {
    fn eq(&self, other: &Self) -> bool {
        match (
            self.clone().try_convert::<Value>(),
            other.clone().try_convert::<Value>(),
        ) {
            (Ok(l), Ok(r)) => l == r,
            _ => false,
        }
    }
}

pub trait LocalResponse: Any + DynClone + Debug + Send + Sync + 'static {}

impl<T> LocalResponse for T where T: Any + DynClone + Debug + Send + Sync + 'static {}

pub trait RemoteResponse: Any + DynClone + Debug + Send + Sync + 'static {
    fn serialize(&self) -> BoxedResult<Value>;
}

impl<T> RemoteResponse for T
where
    T: Any + Serialize + DynClone + Debug + Send + Sync + 'static,
{
    fn serialize(&self) -> BoxedResult<Value> {
        Ok(serde_json::to_value(clone_box(self))?)
    }
}

#[derive(Debug)]
pub enum Response {
    Local(Box<dyn LocalResponse>),
    Remote(Box<dyn RemoteResponse>),
    Value(Value),
}

impl Response {
    pub fn try_convert<T: DeserializeOwned + 'static>(self) -> BoxedResult<T> {
        match self {
            Response::Local(l) => {
                if (l.as_ref() as &dyn Any).is::<T>() {
                    match (l as Box<dyn Any>).downcast::<T>() {
                        Ok(t) => Ok(*t),
                        Err(_) => panic!(
                            "type ids were the same for type {}: should be downcastable",
                            type_name::<T>()
                        ),
                    }
                } else {
                    Err(format!(
                        "cannot convert local response to {} (response = {:?})",
                        type_name::<T>(),
                        l
                    )
                    .to_boxed_error())
                }
            }
            Response::Remote(r) => {
                if (r.as_ref() as &dyn Any).is::<T>() {
                    match (r as Box<dyn Any>).downcast::<T>() {
                        Ok(t) => Ok(*t),
                        Err(_) => panic!(
                            "type ids were the same for type {}: should be downcastable",
                            type_name::<T>()
                        ),
                    }
                } else {
                    let v = r.serialize()?;

                    Ok(serde_json::from_value(v)?)
                }
            }
            Response::Value(v) => Ok(serde_json::from_value(v)?),
        }
    }
}

impl Serialize for Response {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Local(l) => Err(serde::ser::Error::custom(format!(
                "cannot serialize local response {l:?}"
            ))),
            Self::Remote(r) => {
                let v = r.serialize().map_err(serde::ser::Error::custom)?;

                Serialize::serialize(&v, serializer)
            }
            Self::Value(v) => Serialize::serialize(&v, serializer),
        }
    }
}

impl<'de> Deserialize<'de> for Response {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self::Value(Value::deserialize(deserializer)?))
    }
}

impl Clone for Response {
    fn clone(&self) -> Self {
        match self {
            Self::Local(l) => Self::Local(clone_box(l.as_ref())),
            Self::Remote(r) => Self::Remote(clone_box(r.as_ref())),
            Self::Value(v) => Self::Value(v.clone()),
        }
    }
}

impl PartialEq for Response {
    fn eq(&self, other: &Self) -> bool {
        match (
            self.clone().try_convert::<Value>(),
            other.clone().try_convert::<Value>(),
        ) {
            (Ok(l), Ok(r)) => l == r,
            _ => false,
        }
    }
}

impl Default for Response {
    fn default() -> Self {
        Self::Value(Value::Null)
    }
}
