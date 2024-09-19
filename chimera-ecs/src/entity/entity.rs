
use std::any::{TypeId, Any};

use crate::prelude::*;

pub trait EntityParameter<C: Context = ()>: Debug + Send + Sync + 'static {
    fn get_id(&self) -> &Id;
    
    fn get_type(&self) -> TypeId {
        TypeId::of::<Self>()
    }

    fn push(&self, effects: &mut EffectList<C>, to: &Id) -> bool {
        let _ = effects;
        let _ = to;
        false
    }

    fn create_entity(&self) -> Effect<C, EntityReference<C>>;
}

pub trait PureEntity<C: Context = ()> {
    fn new() -> Effect<C, EntityReference<C>>;
}

pub trait ParameterizedEntity<C: Context = ()> {
    type Param: EntityParameter<C>;

    fn new(param: &Self::Param) -> Effect<C, EntityReference<C>>;
}

pub trait SerializableParamEntity<C: Context = ()>: ParameterizedEntity<C>
    where <Self as ParameterizedEntity<C>>::Param: serde::Serialize + serde::de::DeserializeOwned {
    
}

impl<C: Context, T: ParameterizedEntity<C>> SerializableParamEntity<C> for T
    where <T as ParameterizedEntity<C>>::Param: serde::Serialize + serde::de::DeserializeOwned {

}

pub trait StaticallyTypedEntity {
    const TYPE: &'static str;

    fn get_singleton_id() -> Id {
        Id::Singleton(Self::TYPE.to_owned())
    }
}

pub trait Entity<C: Context = ()> where Self: Sync + Send + Debug + Any {
    fn get_id(&self) -> Id;
    fn get_type(&self) -> CowStr;

    fn load(&self, data: Value) -> Effect<C> {
        let _ = data;
        unimplemented!()
    }

    fn save(&self) -> BoxedResult<Option<Value>> {
        Ok(None)
    }

    fn set_parent(&mut self, parent: Option<Id>) -> Effect<C> {
        let _ = parent;
        Effect::default()
    }
    fn on_added(&mut self) -> Effect<C> {
        Effect::default()
    }
    fn on_removed(&mut self) -> Effect<C> {
        Effect::default()
    }
    fn on_pushed(&mut self) -> Effect<C> {
        Effect::default()
    }
    fn on_popped(&mut self) -> Effect<C> {
        Effect::default()
    }
    fn tick(&mut self) -> Effect<C> {
        Effect::default()
    }

    fn handle(&mut self, msg: &mut Message) -> Effect<C> {
        let _ = msg;
        Effect::default()
    }
    
    fn query(&self, query: &mut Query) -> Effect<C, Response> {
        let _ = query;
        Effect::default()
    }

    fn render(&self) -> BoxedResult<Option<Node>> {
        Ok(None)
    }
}

pub type EntityReference<C = ()> = Arc<RwLock<dyn Entity<C>>>;
pub type EntityWeakReference<C = ()> = Weak<RwLock<dyn Entity<C>>>;

pub enum EntityRef<C: Context = ()> {
    Owned(EntityReference<C>),
    Child(EntityWeakReference<C>),
}

impl<C: Context> EntityRef<C> {
    pub fn try_deref(&self) -> BoxedResult<EntityReference<C>> {
        match self {
            EntityRef::Owned(a) => Ok(a.clone()),
            EntityRef::Child(w) => {
                Ok(w.upgrade_or_err("entity")?)
            },
        }
    }
}
