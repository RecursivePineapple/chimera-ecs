use std::{fmt::Debug, sync::Arc};

use chimera_utils::*;
use serde_json::Value;
use uuid::Uuid;

use crate::prelude::*;

#[derive(Debug, Clone)]
pub struct DispatchMessage {
    pub message: Payload,
}

impl From<Payload> for DispatchMessage {
    fn from(message: Payload) -> Self {
        DispatchMessage { message }
    }
}

/// An object that can fanout broadcast to other dispatches,
/// and receives all messages from those same dispatches.
///
/// Given two dispatches, A and B, this is an example.
///
/// - `A.send()` accepts a message from A's owner.
/// - `A.send()` invokes the `A.get_tx()` event, which is connected to the `B.get_rx()` event.
/// - The `B.get_rx()` event calls the relevant code in B's owner.
///
/// The same works in the opposite direction.
///
/// Given three dispatches, A, B, and C, this is another example.
///
/// - `A.send()` accepts a message from A's owner.
/// - `A.send()` invokes the `A.get_tx()` event, which is connected to `B.get_rx()` and `C.get_rx()`.
/// - `B.get_rx()` passes the event to its owner, and `C.get_rx()` does the same.
///
/// This also works in the opposite direction; `B.send()` will pass the message to A's owner and C's owner
/// and vice versa.
///
pub trait Dispatch
where
    Self: Send + Sync,
{
    /// Invokes the `self.get_tx()` event.
    /// This is UNSAFE to call from within an Entity - it will cause a deadlock.
    unsafe fn send(&self, msg: DispatchMessage) -> BoxedResult<()>;

    /// Gets the receive event, which will pass the message to its owner when invoked.
    fn get_rx(&self) -> DynInvokeRef<DispatchMessage>;

    /// Gets the transmit event, which will be called when `self.send()` is invoked.
    fn get_tx(&self) -> SubscriptionWatchRef<DispatchMessage>;

    /// Returns this dispatch's id. Must remain constant.
    fn get_id(&self) -> BoxedResult<Uuid>;

    fn set_permissions(&self, perms: Option<Arc<DispatchPermissions>>) -> BoxedResult<()> {
        let _ = perms;
        Err("this dispatch does not support setting permissions".to_boxed_error())
    }

    /// Returns this dispatch's permissions. Can change throughout its lifetime.
    fn get_permissions(&self) -> BoxedResult<Option<Arc<DispatchPermissions>>> {
        Ok(None)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DispatchData {
    pub id: Uuid,
    pub perms: Option<Arc<DispatchPermissions>>,
}

impl DispatchData {
    pub fn has_tx_permissions(&self, who: Option<&ClientId>) -> bool {
        match self.perms.as_ref() {
            Some(p) => p.tx.includes(who),
            None => true,
        }
    }

    pub fn has_rx_permissions(&self, who: Option<&ClientId>) -> bool {
        match self.perms.as_ref() {
            Some(p) => p.rx.includes(who),
            None => true,
        }
    }
}

impl TryFrom<&dyn Dispatch> for DispatchData {
    type Error = BoxedError;
    fn try_from(value: &dyn Dispatch) -> Result<Self, Self::Error> {
        Ok(Self {
            id: value.get_id()?,
            perms: value.get_permissions()?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DispatchPermissions {
    /// The permission set for sending to this dispatch (client -> server).
    pub tx: PermissionSet,
    /// The permission set for receiving from this dispatch (server -> client).
    pub rx: PermissionSet,
}

pub trait AsDispatchExt {
    fn as_dispatch(&self) -> BoxedResult<SceneProp>;

    fn as_dispatch_with_perms(
        &self,
        tx: PermissionSet,
        rx: PermissionSet,
    ) -> BoxedResult<SceneProp>;

    fn as_tx_dispatch(&self) -> BoxedResult<SceneProp> {
        self.as_dispatch_with_perms(PermissionSet::allow_all(), PermissionSet::deny_all())
    }

    fn as_rx_dispatch(&self) -> BoxedResult<SceneProp> {
        self.as_dispatch_with_perms(PermissionSet::deny_all(), PermissionSet::allow_all())
    }

    fn as_txrx_dispatch(&self) -> BoxedResult<SceneProp> {
        self.as_dispatch_with_perms(PermissionSet::allow_all(), PermissionSet::allow_all())
    }
}

impl<D: Dispatch + 'static> AsDispatchExt for Arc<D> {
    fn as_dispatch(&self) -> BoxedResult<SceneProp> {
        Ok(SceneProp::Dispatch(
            self.clone(),
            DispatchData::try_from(Arc::as_ref(self) as &dyn Dispatch)?,
        ))
    }

    fn as_dispatch_with_perms(
        &self,
        to_server: PermissionSet,
        to_client: PermissionSet,
    ) -> BoxedResult<SceneProp> {
        let mut d = DispatchData::try_from(Arc::as_ref(self) as &dyn Dispatch)?;

        d.perms = Some(Arc::new(DispatchPermissions {
            tx: to_server,
            rx: to_client,
        }));

        Ok(SceneProp::Dispatch(self.clone(), d))
    }
}

impl AsDispatchExt for Arc<dyn Dispatch> {
    fn as_dispatch(&self) -> BoxedResult<SceneProp> {
        Ok(SceneProp::Dispatch(
            self.clone(),
            DispatchData::try_from(Arc::as_ref(self) as &dyn Dispatch)?,
        ))
    }

    fn as_dispatch_with_perms(
        &self,
        to_server: PermissionSet,
        to_client: PermissionSet,
    ) -> BoxedResult<SceneProp> {
        let mut d = DispatchData::try_from(Arc::as_ref(self) as &dyn Dispatch)?;

        d.perms = Some(Arc::new(DispatchPermissions {
            tx: to_server,
            rx: to_client,
        }));

        Ok(SceneProp::Dispatch(self.clone(), d))
    }
}

pub trait WithDispatchPermissionsExt
where
    Self: Sized,
{
    fn with_permissions(self, perms: DispatchPermissions) -> BoxedResult<Self>;

    fn with_tx_rx_whitelist(self, mut whitelist: Vec<ClientId>) -> BoxedResult<Self> {
        whitelist.sort_unstable();

        self.with_permissions(DispatchPermissions {
            rx: PermissionSet::WhiteList(whitelist.clone()),
            tx: PermissionSet::WhiteList(whitelist),
        })
    }

    fn with_owner(self, owner: ClientId) -> BoxedResult<Self> {
        self.with_permissions(DispatchPermissions {
            rx: PermissionSet::WhiteList(vec![owner.clone()]),
            tx: PermissionSet::WhiteList(vec![owner]),
        })
    }
}

impl<D: Dispatch + 'static> WithDispatchPermissionsExt for Arc<D> {
    fn with_permissions(self, perms: DispatchPermissions) -> BoxedResult<Self> {
        self.set_permissions(Some(Arc::new(perms)))?;
        Ok(self)
    }
}

impl WithDispatchPermissionsExt for Arc<dyn Dispatch> {
    fn with_permissions(self, perms: DispatchPermissions) -> BoxedResult<Self> {
        self.set_permissions(Some(Arc::new(perms)))?;
        Ok(self)
    }
}

impl std::fmt::Debug for dyn Dispatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "dyn Dispatch[{}]",
            match self.get_id() {
                Ok(l) => l.to_string(),
                Err(e) => e.to_string(),
            }
        ))
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct DispatchConnection {
    left_sub: Option<Subscription<DispatchMessage>>,
    right_sub: Option<Subscription<DispatchMessage>>,
}

impl DispatchConnection {
    pub fn connect<L, R>(
        left: &Arc<L>,
        right: &Arc<R>,
        l2r: bool,
        r2l: bool,
    ) -> BoxedResult<DispatchConnection>
    where
        L: Dispatch + ?Sized + 'static,
        R: Dispatch + ?Sized + 'static,
    {
        Ok(DispatchConnection {
            left_sub: if l2r {
                Some(left.get_tx().watch(Box::new({
                    let right = Arc::downgrade(right);
                    move |value| {
                        if let Some(right) = right.upgrade() {
                            right.get_rx().invoke(value)?;
                        }
                        Ok(())
                    }
                }))?)
            } else {
                None
            },
            right_sub: if r2l {
                Some(right.get_tx().watch(Box::new({
                    let left = Arc::downgrade(left);
                    move |value| {
                        if let Some(left) = left.upgrade() {
                            left.get_rx().invoke(value)?;
                        }
                        Ok(())
                    }
                }))?)
            } else {
                None
            },
        })
    }
}

pub trait MessageDispatchExt {
    unsafe fn send_payload(&self, payload: Payload) -> BoxedResult<()>;

    unsafe fn send_local<T: LocalPayload>(&self, local: T) -> BoxedResult<()> {
        self.send_payload(Payload::Local(Box::new(local)))
    }
    unsafe fn send_remote<T: RemotePayload>(&self, remote: T) -> BoxedResult<()> {
        self.send_payload(Payload::Remote(Box::new(remote)))
    }
    unsafe fn send_value(&self, value: Value) -> BoxedResult<()> {
        self.send_payload(Payload::Value(value))
    }
}

impl MessageDispatchExt for Arc<dyn Dispatch> {
    unsafe fn send_payload(&self, payload: Payload) -> BoxedResult<()> {
        self.send(payload.into())
            .map_err_explain("could not send message")?;
        Ok(())
    }
}

impl<D: Dispatch> MessageDispatchExt for Arc<D> {
    unsafe fn send_payload(&self, payload: Payload) -> BoxedResult<()> {
        self.send(payload.into())
            .map_err_explain("could not send message")?;
        Ok(())
    }
}
