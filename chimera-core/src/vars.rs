
use std::{sync::Arc, marker::PhantomData, fmt::Debug};

use derivative::Derivative;
use serde::{Serialize, de::DeserializeOwned};
use serde_json::Value;
use chimera_utils::*;
use uuid::Uuid;

use crate::{node::SceneProp, permissions::PermissionSet, prelude::ClientId};

pub type VariableChangeCallback = dyn Fn(&dyn Variable, &Value) + Sync + Send;

pub trait Variable where Self: Send + Sync + Debug {
    fn get(&self) -> BoxedResult<Value>;
    fn set(&self, value: Value) -> BoxedResult<()>;

    fn on_changed(&self) -> SubscriptionWatchRef<Value>;

    fn get_id(&self) -> Uuid;

    fn set_permissions(&self, perms: Option<Arc<VariablePermissions>>) -> BoxedResult<()> {
        let _ = perms;
        Err("this variable does not support setting permissions".to_boxed_error())
    }

    /// Returns this variable's permissions. Can change throughout its lifetime.
    fn get_permissions(&self) -> BoxedResult<Option<Arc<VariablePermissions>>> {
        Ok(None)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct VariableData {
    pub id: Uuid,
    pub perms: Option<Arc<VariablePermissions>>,
}

impl VariableData {
    pub fn get_direction(&self, who: Option<&ClientId>) -> Option<VariableDirection> {
        self.perms.as_ref().and_then(|o| o.get_direction(who))
    }
}

impl TryFrom<&dyn Variable> for VariableData {
    type Error = BoxedError;
    fn try_from(value: &dyn Variable) -> Result<Self, Self::Error> {
        Ok(Self {
            id: value.get_id(),
            perms: value.get_permissions()?
        })
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum VariableDirection {
    Read,
    Write
}

///
/// Controls which clients can interact with a variable.
/// Clients must either be a reader or writer, not both.
/// This must be weakly enforced at the node tree consumer level with a warning. // TODO: actually implement this
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct VariablePermissions {
    /// The permission set for reading and receiving updates for this variable.
    pub read: PermissionSet,
    /// The permission set for writing and sending updates for this variable.
    pub write: PermissionSet,
}

impl VariablePermissions {
    pub fn get_direction(&self, who: Option<&ClientId>) -> Option<VariableDirection> {
        let rx = self.read.includes(who);
        let tx = self.write.includes(who);

        if rx || tx {
            Some(if tx {
                VariableDirection::Write
            } else {
                VariableDirection::Read
            })
        } else {
            None
        }
    }
}

pub trait AsVariableExt {
    fn as_variable(&self) -> BoxedResult<SceneProp>;

    fn as_variable_with_perms(&self, read: PermissionSet, write: PermissionSet) -> BoxedResult<SceneProp>;

    fn as_ro_variable(&self) -> BoxedResult<SceneProp> {
        self.as_variable_with_perms(PermissionSet::allow_all(), PermissionSet::deny_all())
    }

    fn as_wo_variable(&self) -> BoxedResult<SceneProp> {
        self.as_variable_with_perms(PermissionSet::deny_all(), PermissionSet::allow_all())
    }

    fn as_rw_variable(&self) -> BoxedResult<SceneProp> {
        self.as_variable_with_perms(PermissionSet::allow_all(), PermissionSet::allow_all())
    }
}

impl<V: Variable + 'static> AsVariableExt for Arc<V> {
    fn as_variable(&self) -> BoxedResult<SceneProp> {
        Ok(SceneProp::Variable(self.clone(), VariableData::try_from(Arc::as_ref(self) as &dyn Variable)?))
    }

    fn as_variable_with_perms(&self, read: PermissionSet, write: PermissionSet) -> BoxedResult<SceneProp> {
        let mut v = VariableData::try_from(Arc::as_ref(self) as &dyn Variable)?;

        v.perms = Some(Arc::new(VariablePermissions { read, write }));

        Ok(SceneProp::Variable(self.clone(), v))
    }
}

impl AsVariableExt for Arc<dyn Variable> {
    fn as_variable(&self) -> BoxedResult<SceneProp> {
        Ok(SceneProp::Variable(self.clone(), VariableData::try_from(Arc::as_ref(self) as &dyn Variable)?))
    }

    fn as_variable_with_perms(&self, read: PermissionSet, write: PermissionSet) -> BoxedResult<SceneProp> {
        let mut v = VariableData::try_from(Arc::as_ref(self) as &dyn Variable)?;

        v.perms = Some(Arc::new(VariablePermissions { read, write }));

        Ok(SceneProp::Variable(self.clone(), v))
    }
}

pub trait WithVariablePermissionsExt where Self: Sized {
    fn with_permissions(self, perms: VariablePermissions) -> BoxedResult<Self>;

    fn with_read_write_whitelist(self, list: Vec<ClientId>) -> BoxedResult<Self> {
        self.with_permissions(VariablePermissions {
            read: PermissionSet::whitelist(list.iter().cloned()),
            write: PermissionSet::whitelist(list.into_iter())
        })
    }

    fn with_owner(self, owner: ClientId) -> BoxedResult<Self> {
        self.with_permissions(VariablePermissions {
            read: PermissionSet::whitelist(vec![owner.clone()].into_iter()),
            write: PermissionSet::whitelist(vec![owner].into_iter())
        })
    }
}

impl<V: Variable + 'static> WithVariablePermissionsExt for Arc<V> {
    fn with_permissions(self, perms: VariablePermissions) -> BoxedResult<Self> {
        self.set_permissions(Some(Arc::new(perms)))?;
        Ok(self)
    }
}

impl WithVariablePermissionsExt for Arc<dyn Variable> {
    fn with_permissions(self, perms: VariablePermissions) -> BoxedResult<Self> {
        self.set_permissions(Some(Arc::new(perms)))?;
        Ok(self)
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct LocalVariable {
    id: Uuid,
    name: String,
    value: RwLock<Arc<Value>>,
    #[derivative(Debug = "ignore")]
    on_changed: BroadcastEvent<Value>,
    perms: RwLock<Option<Arc<VariablePermissions>>>,
}

impl LocalVariable {
    pub fn new(name: String, initial: Value) -> LocalVariable {
        Self {
            id: Uuid::new_v4(),
            name,
            value: RwLock::new(Arc::new(initial)),
            on_changed: BroadcastEvent::new(),
            perms: RwLock::new(None),
        }
    }

    pub fn map<F: FnOnce(&Value)->R, R>(&self, f: F) -> BoxedResult<R> {
        self.value.with_immut(|v| {
            f(v.as_ref())
        })
    }
}

impl Variable for LocalVariable {
    fn get(&self) -> BoxedResult<Value> {
        Ok(read_lock_with_timeout(&self.value)?.as_ref().clone())
    }

    fn set(&self, value: Value) -> BoxedResult<()> {
        let value = Arc::new(value);

        self.value.with_mut({
            let value = value.clone();
            move |data| {
                *data = value;
    
                Ok(())
            }
        }).flatten()?;

        self.on_changed.invoke(&*value)?;
    
        Ok(())
    }

    fn on_changed(&self) -> SubscriptionWatchRef<Value> {
        &self.on_changed
    }

    fn get_id(&self) -> Uuid {
        self.id.clone()
    }

    fn set_permissions(&self, perms: Option<Arc<VariablePermissions>>) -> BoxedResult<()> {
        *write_lock_with_timeout(&self.perms)? = perms;
        Ok(())
    }

    fn get_permissions(&self) -> BoxedResult<Option<Arc<VariablePermissions>>> {
        Ok(read_lock_with_timeout(&self.perms)?.clone())
    }
}

#[derive(Debug)]
pub struct TypedLocalVariable<T: Serialize + DeserializeOwned + Debug + Send + Sync>(LocalVariable, PhantomData<T>);

impl<T> TypedLocalVariable<T> where T: Serialize + DeserializeOwned + Debug + Send + Sync {
    pub fn new(name: impl Into<String>, value: T) -> BoxedResult<Self> {
        Ok(Self(LocalVariable::new(name.into(), serde_json::to_value(value)?), PhantomData::default()))
    }

    pub fn get_typed(&self) -> BoxedResult<T> {
        Ok(serde_json::from_value(self.0.get()?)?)
    }

    pub fn set_typed(&self, value: T) -> BoxedResult<()> {
        self.0.set(serde_json::to_value(value)?)
    }
}

impl<T> Variable for TypedLocalVariable<T> where T: Serialize + DeserializeOwned + Debug + Send + Sync {
    fn get(&self) -> BoxedResult<Value> {
        self.0.get()
    }

    fn set(&self, value: Value) -> BoxedResult<()> {
        self.0.set(value)
    }

    fn on_changed(&self) -> SubscriptionWatchRef<Value> {
        self.0.on_changed()
    }

    fn get_id(&self) -> Uuid {
        self.0.get_id()
    }

    fn set_permissions(&self, perms: Option<Arc<VariablePermissions>>) -> BoxedResult<()> {
        self.0.set_permissions(perms)
    }

    fn get_permissions(&self) -> BoxedResult<Option<Arc<VariablePermissions>>> {
        self.0.get_permissions()
    }
}
