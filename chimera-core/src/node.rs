
use std::{collections::HashMap, sync::Arc, fmt::Debug};

use derivative::Derivative;
use serde::Serialize;
use serde_json::Value;
use chimera_utils::*;

use crate::{dispatch::{Dispatch, DispatchData}, vars::{Variable, VariableData}, prelude::{PermissionSet, ClientId}};

#[derive(Debug, Clone, Default, PartialEq, Derivative)]
pub struct Node {
    pub scene: String,
    pub props: HashMap<String, SceneProp>,
    pub meta: HashMap<String, Value>,
    pub children: HashMap<String, Node>,
    pub visibility: Option<PermissionSet>,
}

impl Node {
    pub fn is_visible_to(&self, who: Option<&ClientId>) -> bool {
        match &self.visibility {
            Some(p) => p.includes(who),
            None => true
        }
    }
}

#[derive(Clone, Debug)]
pub enum SceneProp {
    Value(Value, Option<PermissionSet>),
    Dispatch(Arc<dyn Dispatch>, DispatchData),
    Variable(Arc<dyn Variable>, VariableData)
}

impl PartialEq for SceneProp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Value(l0, _), Self::Value(r0, _)) => l0 == r0,
            (Self::Dispatch(_, l0), Self::Dispatch(_, r0)) => l0 == r0,
            (Self::Variable(_, l0), Self::Variable(_, r0)) => l0 == r0,
            _ => false,
        }
    }
}

pub trait AsValueExt {
    fn as_value(&self) -> SceneProp;

    fn as_value_with_perms(&self, perms: PermissionSet) -> SceneProp;
}

impl<T: Serialize> AsValueExt for T {
    fn as_value(&self) -> SceneProp {
        SceneProp::Value(serde_json::to_value(self).unwrap(), None)
    }

    fn as_value_with_perms(&self, perms: PermissionSet) -> SceneProp {
        SceneProp::Value(serde_json::to_value(self).unwrap(), Some(perms))
    }
}

pub trait PassiveNodeSource {
    fn get_tree(&self) -> BoxedResult<Option<Arc<Node>>>;

    fn on_changed(&self) -> SubscriptionWatchRef<'_, ()>;
}

pub trait ActiveNodeSource {
    fn on_push(&self) -> SubscriptionWatchRef<'_, Option<Arc<Node>>>;

    fn feed_into<Sink: PassiveNodeSink + Sync + Send + 'static>(&self, sink: impl std::ops::Deref<Target = Sink> + Send + Sync + 'static) -> BoxedResult<Subscription<Option<Arc<Node>>>> {
        let sink = Mutex::new(sink);

        self.on_push().watch(Box::new(move |tree| {
            sink.with_mut(|sink| {
                sink.deref().push(tree.clone())
            })??;

            Ok(())
        }))
    }
}

pub trait ActiveNodeSourceExt: ActiveNodeSource + Sized {
    fn as_passive(self) -> BoxedResult<NodeSourceBuffer<Self>> {
        NodeSourceBuffer::new(self)
    }
}

impl<Src: ActiveNodeSource + Sized> ActiveNodeSourceExt for Src {

}

pub trait PassiveNodeSink {
    fn push(&self, tree: Option<Arc<Node>>) -> BoxedResult;
}

pub trait ActiveNodeSink {
    fn feed_from<Src: PassiveNodeSource + Sync + Send + 'static>(&mut self, source: Src) -> BoxedResult;
}

pub struct NodeSourceBuffer<Src: ActiveNodeSource> {
    _source: Src,
    _sub: Subscription<Option<Arc<Node>>>,
    tree: Arc<RwLock<Option<Arc<Node>>>>,
    changed: BroadcastEvent<()>,
}

impl<Src: ActiveNodeSource> NodeSourceBuffer<Src> {
    pub fn new(source: Src) -> BoxedResult<Self> {

        let changed = BroadcastEvent::new();
        let tree = Arc::new(RwLock::new(None));
        
        let sub = source.on_push().watch({
            let changed = changed.clone();
            let tree = tree.clone();

            Box::new(move |node_tree| {
                tree.with_mut(|tree| {
                    *tree = node_tree.clone();
                })?;

                changed.invoke(&())?;

                Ok(())
            })
        })?;

        Ok(Self {
            _source: source,
            _sub: sub,
            tree,
            changed,
        })
    }
}

impl<Src: ActiveNodeSource> PassiveNodeSource for NodeSourceBuffer<Src> {
    fn get_tree(&self) -> BoxedResult<Option<Arc<Node>>> {
        self.tree.with_immut(|tree| tree.clone())
    }

    fn on_changed(&self) -> SubscriptionWatchRef<'_, ()> {
        &self.changed
    }
}
