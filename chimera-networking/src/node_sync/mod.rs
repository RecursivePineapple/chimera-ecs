use std::{collections::HashMap, fmt::Debug, sync::Arc};

use chimera_core::prelude::*;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use chimera_utils::*;
use uuid::Uuid;

#[cfg(test)]
mod node_sync_tests;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ScenePropData {
    Value(Value),
    Dispatch { id: Uuid, rx: bool, tx: bool },
    Variable { id: Uuid, rx: bool, tx: bool },
}

impl ScenePropData {
    pub fn try_from(ident: &Vec<ClientId>, v: &SceneProp) -> Option<Self> {
        let rx = v.has_rx_permissions(ident);
        let tx = v.has_tx_permissions(ident);

        if rx || tx {
            Some(match v {
                SceneProp::Value(v, _) => ScenePropData::Value(v.clone()),
                SceneProp::Dispatch(_, d) => ScenePropData::Dispatch {
                    id: d.id.clone(),
                    rx,
                    tx,
                },
                SceneProp::Variable(_, v) => ScenePropData::Variable {
                    id: v.id.clone(),
                    rx,
                    tx,
                },
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct NodeData {
    pub scene: String,
    pub props: HashMap<String, ScenePropData>,
    pub meta: HashMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PatchOperation<T> {
    Insert(T),
    Delete,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct NodeDataPatch {
    pub scene: Option<String>,
    pub props: Option<HashMap<String, PatchOperation<ScenePropData>>>,
    pub meta: Option<HashMap<String, PatchOperation<Value>>>,
}

impl NodeData {
    pub fn new(ident: &Vec<ClientId>, node: &Node) -> Self {
        Self {
            scene: node.scene.clone(),
            props: node
                .props
                .iter()
                .filter_map(|(k, v)| ScenePropData::try_from(ident, v).map(|d| (k.clone(), d)))
                .collect(),
            meta: node.meta.clone(),
        }
    }

    pub fn diff(&self, other: &Self) -> Option<NodeDataPatch> {
        let mut p = NodeDataPatch::default();

        if self.scene != other.scene {
            p.scene = Some(other.scene.clone());
        }

        for (k, d) in delta_hash_maps(&self.props, &other.props) {
            let d = match d {
                HashMapDeltaChange::Left(_) => PatchOperation::Delete,
                HashMapDeltaChange::Changed(_, r) | HashMapDeltaChange::Right(r) => {
                    PatchOperation::Insert(r.clone())
                }
            };

            if p.props.is_none() {
                p.props = Some(HashMap::new());
            }

            p.props.as_mut().unwrap().insert(k.clone(), d);
        }

        for (k, d) in delta_hash_maps(&self.meta, &other.meta) {
            let d = match d {
                HashMapDeltaChange::Left(_) => PatchOperation::Delete,
                HashMapDeltaChange::Changed(_, r) | HashMapDeltaChange::Right(r) => {
                    PatchOperation::Insert(r.clone())
                }
            };

            if p.meta.is_none() {
                p.meta = Some(HashMap::new());
            }

            p.meta.as_mut().unwrap().insert(k.clone(), d);
        }

        if p.scene.is_some() || p.props.is_some() || p.meta.is_some() {
            Some(p)
        } else {
            None
        }
    }

    pub fn to_node<T: TransportStateFactory>(
        self,
        transport: &T,
        defaults: &HashMap<Uuid, Value>,
    ) -> BoxedResult<Node> {
        let mut n = Node::default();

        n.scene = self.scene;
        n.meta = self.meta;

        for (k, v) in self.props {
            let p = NodeDataPatch::get_prop(transport, defaults, v)
                .map_err_explain_with(|| format!("could not instantiate SceneProp {k}"))?;
            n.props.insert(k, p);
        }

        Ok(n)
    }
}

impl NodeDataPatch {
    pub fn patch<T: TransportStateFactory>(
        self,
        node: &mut Node,
        transport: &T,
        defaults: &HashMap<Uuid, Value>,
    ) -> BoxedResult {
        if let Some(scene) = self.scene {
            node.scene = scene;
        }

        if let Some(props) = self.props {
            for (k, d) in props {
                match d {
                    PatchOperation::Insert(v) => {
                        let p =
                            Self::get_prop(transport, defaults, v).map_err_explain_with(|| {
                                format!("could not instantiate SceneProp {k}")
                            })?;
                        node.props.insert(k, p);
                    }
                    PatchOperation::Delete => {
                        node.props.remove(&k);
                    }
                }
            }
        }

        if let Some(meta) = self.meta {
            for (k, d) in meta {
                match d {
                    PatchOperation::Insert(v) => {
                        node.meta.insert(k, v);
                    }
                    PatchOperation::Delete => {
                        node.meta.remove(&k);
                    }
                }
            }
        }

        Ok(())
    }

    pub fn get_prop<T: TransportStateFactory>(
        transport: &T,
        defaults: &HashMap<Uuid, Value>,
        p: ScenePropData,
    ) -> BoxedResult<SceneProp> {
        Ok(match p {
            ScenePropData::Value(v) => SceneProp::Value(v.clone(), None),
            ScenePropData::Dispatch { id, rx, tx } => SceneProp::Dispatch(
                transport.get_dispatch(&id, rx, tx)?,
                DispatchData {
                    id,
                    perms: Some(Arc::new(DispatchPermissions {
                        rx: PermissionSet::allow(rx),
                        tx: PermissionSet::allow(tx),
                    })),
                },
            ),
            ScenePropData::Variable { id, rx, tx } => {
                let v = defaults.get(&id).ok_or_else(|| {
                    format!("could not find default value for new variable {id}").to_boxed_error()
                })?;

                SceneProp::Variable(
                    transport.get_variable(&id, rx, tx, v.clone())?,
                    VariableData {
                        id,
                        perms: Some(Arc::new(VariablePermissions {
                            read: PermissionSet::allow(rx),
                            write: PermissionSet::allow(tx),
                        })),
                    },
                )
            }
        })
    }
}

trait ScenePropsPermissions {
    /// Data coming from the client into the server.
    fn has_tx_permissions(&self, who: &Vec<ClientId>) -> bool;
    /// Data coming from the server into the client.
    fn has_rx_permissions(&self, who: &Vec<ClientId>) -> bool;
}

impl ScenePropsPermissions for SceneProp {
    fn has_tx_permissions(&self, who: &Vec<ClientId>) -> bool {
        if who.len() == 0 {
            match self {
                SceneProp::Value(_, _) => false,
                SceneProp::Dispatch(_, data) => match &data.perms {
                    Some(perms) => perms.tx.includes(None),
                    None => true,
                },
                SceneProp::Variable(_, data) => match &data.perms {
                    Some(perms) => perms.write.includes(None),
                    None => true,
                },
            }
        } else {
            who.iter().any(|id| match self {
                SceneProp::Value(_, _) => false,
                SceneProp::Dispatch(_, data) => match &data.perms {
                    Some(perms) => perms.tx.includes(Some(id)),
                    None => true,
                },
                SceneProp::Variable(_, data) => match &data.perms {
                    Some(perms) => perms.write.includes(Some(id)),
                    None => true,
                },
            })
        }
    }

    fn has_rx_permissions(&self, who: &Vec<ClientId>) -> bool {
        if who.len() == 0 {
            match self {
                SceneProp::Value(_, None) => true,
                SceneProp::Value(_, Some(perms)) => perms.includes(None),
                SceneProp::Dispatch(_, data) => match &data.perms {
                    Some(perms) => perms.rx.includes(None),
                    None => true,
                },
                SceneProp::Variable(_, data) => match &data.perms {
                    Some(perms) => perms.read.includes(None),
                    None => true,
                },
            }
        } else {
            who.iter().any(|id| match self {
                SceneProp::Value(_, None) => true,
                SceneProp::Value(_, Some(perms)) => perms.includes(Some(id)),
                SceneProp::Dispatch(_, data) => match &data.perms {
                    Some(perms) => perms.rx.includes(Some(id)),
                    None => true,
                },
                SceneProp::Variable(_, data) => match &data.perms {
                    Some(perms) => perms.read.includes(Some(id)),
                    None => true,
                },
            })
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum NodeDiff {
    Same,
    Delete,
    Merge {
        data: Option<NodeDataPatch>,
        children: HashMap<String, NodeDiff>,
    },
    Replace {
        data: NodeData,
        children: HashMap<String, NodeDiff>,
    },
}

#[derive(Debug)]
enum SyncProp {
    Value(Option<PermissionSet>),
    Dispatch(Arc<dyn Dispatch>, DispatchData),
    Variable(Arc<dyn Variable>, VariableData),
}

impl PartialEq<SceneProp> for SyncProp {
    fn eq(&self, other: &SceneProp) -> bool {
        match (self, other) {
            (Self::Value(lp), SceneProp::Value(_, rp)) => lp == rp,
            (Self::Dispatch(_, ld), SceneProp::Dispatch(_, rd)) => ld == rd,
            (Self::Variable(_, lv), SceneProp::Variable(_, rv)) => lv == rv,
            _ => false,
        }
    }
}

#[derive(Debug, Default)]
struct SyncState {
    pub data: Option<NodeData>,
    pub props: HashMap<String, SyncProp>,
    pub children: HashMap<String, SyncState>,
}

#[derive(Debug, Clone)]
pub enum SubDiff {
    ConnectDispatch {
        d: Arc<dyn Dispatch>,
        rx: bool,
        tx: bool,
    },
    DisconnectDispatch(Arc<dyn Dispatch>),
    ConnectVariable {
        v: Arc<dyn Variable>,
        rx: bool,
        tx: bool,
    },
    DisconnectVariable(Arc<dyn Variable>),
}

pub trait TransportStateFactory
where
    Self: Sync + Send,
{
    fn get_dispatch(&self, id: &Uuid, rx: bool, tx: bool) -> BoxedResult<Arc<dyn Dispatch>>;
    fn get_variable(
        &self,
        id: &Uuid,
        rx: bool,
        tx: bool,
        default: Value,
    ) -> BoxedResult<Arc<dyn Variable>>;
}

#[derive(Debug)]
pub struct NodeDiffer
where
    Self: Sync + Send,
{
    state: Option<SyncState>,
    pub ident: Vec<ClientId>,
}

impl NodeDiffer {
    pub fn new() -> Self {
        Self {
            state: None,
            ident: Vec::new(),
        }
    }

    pub fn push_node_tree(
        &mut self,
        tree_root: Option<&Node>,
    ) -> BoxedResult<(NodeDiff, Vec<SubDiff>)> {
        let prev = self.state.take();

        let (next, diff, subs_diff) = self.visit_node(prev, tree_root)?;

        self.state = next;

        Ok((diff, subs_diff))
    }

    fn visit_node(
        &self,
        state: Option<SyncState>,
        mut node: Option<&Node>,
    ) -> BoxedResult<(Option<SyncState>, NodeDiff, Vec<SubDiff>)> {
        if self.ident.len() > 0
            && let Some(n) = &node
            && let Some(visibility) = n.visibility.as_ref()
        {
            if !self.ident.iter().any(|who| visibility.includes(Some(who))) {
                node = None;
            }
        }

        match node {
            None => {
                if let Some(mut state) = state {
                    let mut subs_diff = self.diff_props(&mut state, None)?;
                    let _ = self.diff_children(&mut state, None, &mut subs_diff)?;

                    Ok((None, NodeDiff::Delete, subs_diff))
                } else {
                    Ok((None, NodeDiff::Same, Vec::new()))
                }
            }
            Some(node) => {
                let mut state = state.unwrap_or_default();
                let data = NodeData::new(&self.ident, node);

                let mut subs_diff = self.diff_props(&mut state, Some(node))?;
                let children_diff = self.diff_children(&mut state, Some(node), &mut subs_diff)?;

                let diff = match state.data.as_ref() {
                    Some(old) => {
                        let patch = old.diff(&data);

                        if patch.is_some() || children_diff.len() > 0 {
                            state.data = Some(data);
                            NodeDiff::Merge {
                                data: patch,
                                children: children_diff,
                            }
                        } else {
                            NodeDiff::Same
                        }
                    }
                    None => {
                        state.data = Some(data.clone());
                        NodeDiff::Replace {
                            data,
                            children: children_diff,
                        }
                    }
                };

                Ok((Some(state), diff, subs_diff))
            }
        }
    }

    fn diff_props(&self, state: &mut SyncState, node: Option<&Node>) -> BoxedResult<Vec<SubDiff>> {
        let mut diffs = Vec::new();

        let keys = state.props.keys();

        let keys = match node {
            Some(n) => keys
                .chain(n.props.keys())
                .sorted()
                .dedup()
                .cloned()
                .collect_vec(),
            None => keys.cloned().collect_vec(),
        };

        for key in keys {
            let rhs = node.and_then(|node| {
                node.props.get(&key).and_then(|p| {
                    let rx = p.has_rx_permissions(&self.ident);
                    let tx = p.has_tx_permissions(&self.ident);

                    if rx || tx {
                        Some((p, rx, tx))
                    } else {
                        None
                    }
                })
            });

            match (state.props.get(&key), rhs) {
                (Some(_), None) => {
                    diffs.extend(self.remove_prop(state, &key).into_iter());
                }
                (None, Some((curr, rx, tx))) => {
                    diffs.extend(self.add_prop(state, &key, curr, rx, tx).into_iter());
                }
                (Some(prev), Some((curr, rx, tx))) if prev != curr => {
                    diffs.extend(self.remove_prop(state, &key).into_iter());
                    diffs.extend(self.add_prop(state, &key, curr, rx, tx).into_iter());
                }
                _ => {}
            }
        }

        Ok(diffs)
    }

    fn remove_prop(&self, state: &mut SyncState, key: &String) -> Option<SubDiff> {
        let value = state.props.remove(key).expect("SyncState to have key");

        match value {
            SyncProp::Value(_) => None,
            SyncProp::Dispatch(d, _) => Some(SubDiff::DisconnectDispatch(d.clone())),
            SyncProp::Variable(v, _) => Some(SubDiff::DisconnectVariable(v.clone())),
        }
    }

    fn add_prop(
        &self,
        state: &mut SyncState,
        key: &String,
        value: &SceneProp,
        rx: bool,
        tx: bool,
    ) -> Option<SubDiff> {
        match value {
            SceneProp::Value(_, perms) => {
                state
                    .props
                    .insert(key.clone(), SyncProp::Value(perms.clone()));

                None
            }
            SceneProp::Dispatch(d, data) => {
                state
                    .props
                    .insert(key.clone(), SyncProp::Dispatch(d.clone(), data.clone()));

                Some(SubDiff::ConnectDispatch {
                    d: d.clone(),
                    rx,
                    tx,
                })
            }
            SceneProp::Variable(v, data) => {
                state
                    .props
                    .insert(key.clone(), SyncProp::Variable(v.clone(), data.clone()));

                Some(SubDiff::ConnectVariable {
                    v: v.clone(),
                    rx,
                    tx,
                })
            }
        }
    }

    fn diff_children(
        &self,
        state: &mut SyncState,
        node: Option<&Node>,
        subs_diff: &mut Vec<SubDiff>,
    ) -> BoxedResult<HashMap<String, NodeDiff>> {
        let mut children_diff = HashMap::new();

        let keys = state.children.keys();

        let keys = match node {
            Some(n) => keys
                .chain(n.children.keys())
                .sorted()
                .dedup()
                .cloned()
                .collect_vec(),
            None => keys.cloned().collect_vec(),
        };

        for key in keys {
            let prev = state.children.remove(&key);
            let curr = node
                .and_then(|node| node.children.get(&key))
                .filter(|child| {
                    child.visibility.is_none()
                        || self.ident.iter().any(|id| child.is_visible_to(Some(id)))
                });

            if prev.is_some() || curr.is_some() {
                let (s, diff, mut subs_diff_child) = self.visit_node(prev, curr)?;

                if let Some(s) = s {
                    state.children.insert(key.clone(), s);
                }

                if !matches!(diff, NodeDiff::Same) {
                    children_diff.insert(key, diff);
                }

                subs_diff.append(&mut subs_diff_child);
            }
        }

        Ok(children_diff)
    }
}

#[derive(Debug)]
pub struct NodePatcher {
    tree: Option<Node>,
}

impl NodePatcher {
    pub fn new() -> Self {
        Self { tree: None }
    }

    pub fn get_tree(&self) -> Option<&Node> {
        self.tree.as_ref()
    }

    pub fn push_node_diff<T: TransportStateFactory>(
        &mut self,
        transport: &T,
        diff_root: NodeDiff,
        defaults: &HashMap<Uuid, Value>,
    ) -> BoxedResult<Option<&Node>> {
        let node = self.tree.take();

        self.tree = self.visit_node(transport, node, diff_root, defaults)?;

        Ok(self.get_tree())
    }

    fn visit_node<T: TransportStateFactory>(
        &self,
        transport: &T,
        node: Option<Node>,
        diff: NodeDiff,
        defaults: &HashMap<Uuid, Value>,
    ) -> BoxedResult<Option<Node>> {
        match diff {
            NodeDiff::Same => Ok(node),
            NodeDiff::Delete => Ok(None),
            NodeDiff::Replace { data, children } => {
                let mut node = data
                    .to_node(transport, defaults)
                    .map_err_explain("could not construct node in replace operation")?;

                for (key, diff) in children.into_iter() {
                    let child = node.children.remove(&key);

                    if let Some(cn) = self.visit_node(transport, child, diff, defaults)? {
                        node.children.insert(key.clone(), cn);
                    }
                }

                Ok(Some(node))
            }
            NodeDiff::Merge { data, children } => {
                let mut node = node.unwrap_or_default();

                if let Some(patch) = data {
                    patch
                        .patch(&mut node, transport, defaults)
                        .map_err_explain("could not apply node patch")?;
                }

                for (key, diff) in children.into_iter() {
                    let child = node.children.remove(&key);

                    if let Some(cn) = self.visit_node(transport, child, diff, defaults)? {
                        node.children.insert(key.clone(), cn);
                    }
                }

                Ok(Some(node))
            }
        }
    }
}
