
use derivative::Derivative;

use crate::prelude::*;

type MapMessage = dyn Fn(&DispatchMessage)->Message + Sync + Send + 'static;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct EntityDispatch {
    id: Uuid,
    
    rx: Delegate<DispatchMessage>,
    tx: BroadcastEvent<DispatchMessage>,

    perms: RwLock<Option<Arc<DispatchPermissions>>>,
}

impl EntityDispatch {
    pub fn new<C: Context>(e: Weak<EntityList<C>>, eid: Id) -> Arc<Self> {
        let sid = Uuid::new_v4();

        Arc::new(Self {
            id: sid.clone(),
            
            rx: Delegate::new(Box::new(move |value| {
                let mut msg = Message::to(eid.clone());
                msg.sender = MessageSender::Dispatch(sid.clone());
                msg.payload = vec![value.message.clone()];

                e.upgrade_or_err("connected dispatch")?.send(msg)?;
                Ok(())
            })),
            tx: BroadcastEvent::new(),

            perms: RwLock::new(None),
        })
    }
    
    pub fn new_with_message(e: Weak<EntityList>, msg: Box<MapMessage>) -> Arc<Self> {
        Arc::new(Self {
            id: Uuid::new_v4(),
            
            rx: Delegate::new(Box::new(move |value| {
                e.upgrade_or_err("connected dispatch")?.send((msg)(value))?;
                Ok(())
            })),
            tx: BroadcastEvent::new(),

            perms: RwLock::new(None),
        })
    }
}

impl Dispatch for EntityDispatch {
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
        Ok(read_lock_with_timeout(&self.perms)?.clone())
    }

    fn set_permissions(&self, perms: Option<Arc<DispatchPermissions>>) -> BoxedResult<()> {
        *write_lock_with_timeout(&self.perms)? = perms;
        Ok(())
    }
}
