use std::sync::Arc;

use uuid::Uuid;

use chimera_utils::*;

use crate::prelude::*;

#[derive(Debug)]
pub struct DummyDispatch {
    pub id: Uuid,
    pub recv: Arc<Mutex<Vec<Payload>>>,
    pub perms: Mutex<Option<Arc<DispatchPermissions>>>,

    tx: BroadcastEvent<DispatchMessage>,
    rx: Delegate<DispatchMessage>,
}

impl DummyDispatch {
    pub fn new() -> Arc<Self> {
        let l = Arc::new(Mutex::new(Vec::new()));

        Arc::new(Self {
            id: Uuid::new_v4(),
            recv: l.clone(),
            perms: Mutex::new(None),
            tx: BroadcastEvent::new(),
            rx: Delegate::new(Box::new(move |value| {
                lock_with_timeout(&*l)?.push(value.message.clone());
                Ok(())
            })),
        })
    }

    pub fn assert_messages(&self, msgs: Vec<Payload>) {
        assert_eq!(&*lock_with_timeout(&self.recv).unwrap(), &msgs);
    }

    pub fn clear_messages(&self) {
        lock_with_timeout(&self.recv).unwrap().clear();
    }
}

impl Dispatch for DummyDispatch {
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

    fn get_permissions(&self) -> BoxedResult<Option<Arc<DispatchPermissions>>> {
        Ok(lock_with_timeout(&self.perms)?.clone())
    }

    fn set_permissions(&self, perms: Option<Arc<DispatchPermissions>>) -> BoxedResult<()> {
        *lock_with_timeout(&self.perms)? = perms;
        Ok(())
    }
}
