use std::{collections::HashMap, sync::{Arc, Weak}, fmt::Debug};

use derivative::Derivative;
use gdnative::{prelude::{*, Node as GDNode}, sys::{godot_property_hint, godot_property_usage_flags_GODOT_PROPERTY_USAGE_STORAGE as PROPERTY_USAGE_STORAGE, godot_variant_type_GODOT_VARIANT_TYPE_ARRAY}};
use tracing::{error, debug};
use chimera_utils::*;
use uuid::Uuid;
use serde_json::Value;

use crate::prelude::{Dispatch, Variable, DispatchConnection, DispatchData, VariableData, VariableValue};
use crate::node::{Node, SceneProp};

#[derive(thiserror::Error, Debug)]
pub enum NodeApplyError {
    #[error("could not find parent node {0}")]
    MissingParent(String),
    #[error("could not find scene '{0}'")]
    BadScenePath(String),
    #[error("resource '{0}' was not a PackedScene")]
    BadResourceType(String),
    #[error("scene '{0}' has no root node")]
    BadSceneRoot(String),
    #[error("error setting property")]
    SetProperty(#[from] GodotError),
    #[error("error converting value for property '{0}' to type '{1}' (value = '{2}')")]
    BadPropertyValue(String, String, Value),
    #[error("scene '{0}' is missing a script")]
    MissingScript(String),
    #[error("scene '{0}' is missing the property '{1}'")]
    MissingProperty(String, String),
    #[error("scene '{0}' is missing a script with the function set_children(Dictionary)")]
    MissingSetChildren(String),
    #[error("error in scene '{0}' while applying child '{1}': {2}")]
    ChildError(String, String, BoxedError),
}

#[derive(Debug)]
enum NodeProp {
    Value(serde_json::Value),
    Dispatch(DispatchData, Arc<dyn Dispatch>, Arc<GodotDispatch>, DispatchConnection),
    Variable(VariableData, Arc<dyn Variable>, Instance<GodotVariable>)
}

impl PartialEq for NodeProp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Value(l0), Self::Value(r0)) => l0 == r0,
            (Self::Dispatch(l0, _, _, _), Self::Dispatch(r0, _, _, _)) => l0 == r0,
            (Self::Variable(l0, _, _), Self::Variable(r0, _, _)) => l0 == r0,
            _ => false,
        }
    }
}

impl PartialEq<SceneProp> for NodeProp {
    fn eq(&self, other: &SceneProp) -> bool {
        match (self, other) {
            (Self::Value(l0), SceneProp::Value(r0)) => l0 == r0,
            (Self::Dispatch(l0, _, _, _), SceneProp::Dispatch(_, r0)) => l0 == r0,
            (Self::Variable(l0, _, _), SceneProp::Variable(_, r0)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Derivative, Default)]
#[derivative(Debug)]
#[cfg_attr(test, derivative(PartialEq))]
struct GNodeState {
    pub scene: String,

    pub props: HashMap<String, NodeProp>,

    pub children: HashMap<String, (GNodeState, Ref<GDNode>)>,

    pub prop_cache: Option<(i64, HashMap<String, PropertyInfo>)>,
}

enum NodeResult {
    Keep,
    Replace(Ref<GDNode>)
}

type PropertyInfo = (VariantType, Option<Vec<VariantType>>);

fn get_object_properties(obj: &Object) -> HashMap<String, PropertyInfo> {
    obj.get_property_list()
        .iter()
        .filter_map(process_property)
        .collect::<HashMap<_, _>>()
}

fn process_property(d: Variant) -> Option<(String, PropertyInfo)> {
    d.to::<Dictionary>().and_then(|d| {
        if d.get("usage")?.to::<godot_property_hint>()? & PROPERTY_USAGE_STORAGE == PROPERTY_USAGE_STORAGE {
            let name = d.get("name")?.to::<String>()?;
            let ptype = d.get("type")?.to::<u32>()?;

            let array_types = if ptype == godot_variant_type_GODOT_VARIANT_TYPE_ARRAY {
                let hint_str = d.get("hint_string")?.to::<String>()?
                    .split('/').next().unwrap_or_else(|| panic!("Expected Array hint_string for {name} to contain text prior to a slash (or no slash)."))
                    .split_terminator(':')
                    .map(|s| s.parse::<u32>())
                    .try_collect::<Vec<_>>()
                    .unwrap_or_else(|_| panic!("Expected Array hint_string for {name} to contain colon separated ints."))
                    .iter()
                    .map(|i| VariantType::from_sys(*i))
                    .collect::<Vec<_>>();

                Some(hint_str)
            } else {
                None
            };

            Some((name, (VariantType::from_sys(ptype), array_types)))
        } else {
            None
        }
    })
}

pub struct MountPoint {
    node_name: String,
    gstate: Option<GNodeState>,
}

impl MountPoint {
    pub fn new(node_name: impl Into<String>) -> Self {
        MountPoint {
            node_name: node_name.into(),
            gstate: None
        }
    }

    #[profiled]
    pub unsafe fn redraw(&mut self, root: &GDNode, tree: Option<Node>) -> BoxedResult<()> {

        let mut cnode = root.get_node_or_null(&self.node_name);
        
        let tree = match tree {
            Some(n) => n,
            None => {
                if let Some(n) = cnode {
                    let r = n.assume_safe();

                    if let Some(p) = r.get_parent() {
                        p.assume_safe().remove_child(r);
                    }

                    r.queue_free();
                }

                return Ok(())
            }
        };

        if cnode.is_none() {
            let n = tree.create()?;

            let ppath = &self.node_name[..self.node_name.rfind('/').unwrap_or(self.node_name.len())];
            let parent = root.get_node_or_null(ppath)
                .ok_or_else(|| NodeApplyError::MissingParent(ppath.to_owned()))?;

            let parent = parent.assume_safe();
            let new_node = n.assume_safe();

            new_node.set_name(&self.node_name[self.node_name.rfind('/').map(|i| i + 1).unwrap_or(0)..]);
            parent.add_child(new_node, false);

            if let Some(owner) = parent.owner() {
                new_node.set_owner(owner);
            }

            cnode = Some(n);
        }

        let (state, result) = tree.apply(self.gstate.take(), cnode)?;
        
        self.gstate = Some(state);

        if let NodeResult::Replace(new_node) = result {
            if let Some(n) = cnode {
                let r = n.assume_safe();

                if let Some(p) = r.get_parent() {
                    p.assume_safe().remove_child(r);
                }

                r.queue_free();
            }

            let ppath = &self.node_name[..self.node_name.rfind('/').unwrap_or(self.node_name.len())];
            let parent = root.get_node_or_null(ppath)
                .ok_or_else(|| NodeApplyError::MissingParent(ppath.to_owned()))?;

            let parent = parent.assume_safe();
            let new_node = new_node.assume_safe();

            new_node.set_name(&self.node_name[self.node_name.rfind('/').map(|i| i + 1).unwrap_or(0)..]);
            parent.add_child(new_node, false);

            if let Some(owner) = parent.owner() {
                new_node.set_owner(owner);
            }
        }

        Ok(())
    }
}

impl Node {
    unsafe fn apply(&self, prev: Option<GNodeState>, n: Option<Ref<GDNode>>) -> BoxedResult<(GNodeState, NodeResult)> {
        let mut state = prev.unwrap_or_default();

        let node = if self.scene == state.scene {
            match n {
                Some(n) => n,
                None => self.create()?
            }
        } else {
            state.scene = self.scene.clone();    
            self.create()?
        };

        let node_ref = node.assume_safe().as_ref();

        self.sync_props(&mut state, node_ref)?;

        if node_ref.has_method("set_children") {
            self.sync_children(&mut state, node_ref)?;
        }

        Ok((state, if Some(node) == n {
            NodeResult::Keep
        } else {
            NodeResult::Replace(node)
        }))
    }

    unsafe fn create(&self) -> Result<Ref<GDNode>, NodeApplyError> {
        let scene = gdnative::api::ResourceLoader::godot_singleton()
            .load(&self.scene, "", false)
            .ok_or_else(|| NodeApplyError::BadScenePath(self.scene.clone()))?;

        let ps = scene.cast::<PackedScene>()
            .ok_or_else(|| NodeApplyError::BadResourceType(self.scene.clone()))?;

        let n = ps.assume_safe()
            .instance(PackedScene::GEN_EDIT_STATE_DISABLED)
            .ok_or_else(|| NodeApplyError::BadSceneRoot(self.scene.clone()))?;
        
        n.assume_safe().set_name(self.path[self.path.as_str().rfind('/').unwrap_or(0)..].to_owned());

        Ok(n)
    }

    unsafe fn sync_props(&self, state: &mut GNodeState, node: &GDNode) -> BoxedResult<()> {
        for (name, delta) in delta_hash_maps2(&state.props, &self.props) {
            match delta {
                utils::HashMapDeltaChange::Left(_) => {
                    let old_value = state.props.remove(&name).unwrap();
                    self.remove_prop(&*state, node, &name, old_value)?;
                },
                utils::HashMapDeltaChange::Changed(_, _) => {
                    let old_value = state.props.remove(&name).unwrap();
                    let new_value = self.props.get(&name).unwrap();

                    if !matches!(old_value, NodeProp::Value(_)) {
                        self.remove_prop(state, node, &name, old_value)?;
                    }

                    let p = self.setup_prop(state, node, &name, new_value)?;

                    state.props.insert(name.clone(), p);
                },
                utils::HashMapDeltaChange::Right(_) => {
                    let new_value = self.props.get(&name).unwrap();

                    let p = self.setup_prop(state, node, &name, new_value)?;
                    
                    state.props.insert(name.clone(), p);
                },
            }
        }

        Ok(())
    }

    unsafe fn remove_prop(&self, _state: &GNodeState, node: &GDNode, name: &String, old_value: NodeProp) -> BoxedResult<()> {
        match old_value {
            NodeProp::Value(_) => {
                node.set(name, ().owned_to_variant());
                Ok(())
            },
            NodeProp::Dispatch(_, _, _, conn) => {
                node.set(name, ().owned_to_variant());

                drop(conn);

                Ok(())
            },
            NodeProp::Variable(_, _, gv) => {
                node.set(name, ().owned_to_variant());

                gv.assume_safe().map_mut(|d, _| {
                    d.reset()
                })?;
                
                Ok(())
            },
        }
    }

    unsafe fn setup_prop(&self, state: &mut GNodeState, node: &GDNode, name: &String, new_value: &SceneProp) -> BoxedResult<NodeProp> {
        match new_value {
            SceneProp::Value(value) => {
                let node_props = if let Some((nid, props)) = &state.prop_cache && nid == &node.get_instance_id() {
                    props
                } else {
                    state.prop_cache = Some((node.get_instance_id(), get_object_properties(node)));
                    &state.prop_cache.as_ref().unwrap().1
                };

                match node_props.get(name) {
                    None => {
                        Err(Box::new(NodeApplyError::MissingProperty(self.scene.clone(), name.clone())))
                    },
                    Some((ptype, array_types)) => {
                        let v = serde_to_variant(value, ptype, array_types.as_ref().map(|a| &a[..]))
                            .ok_or_else(|| NodeApplyError::BadPropertyValue(name.clone(), ptype.name().to_owned(), value.clone()))?;
                        
                        node.set(name, v);

                        Ok(NodeProp::Value(value.clone()))
                    },
                }
            },
            SceneProp::Dispatch(d, data) => {

                let obj = DispatchObject::new_instance().into_shared();

                let gd = Arc::new(GodotDispatch::new(obj.clone(), data.id.clone()));

                let conn = DispatchConnection::connect(d, &gd, true, true)?;

                node.set(name, obj);

                Ok(NodeProp::Dispatch(data.clone(), d.clone(), gd, conn))
            },
            SceneProp::Variable(v, data) => {

                let obj = GodotVariable::new_instance().into_shared();
                obj.assume_safe().map_mut(|d, _| {
                    d.init(obj.clone(), v)
                })??;

                node.set(name, obj.clone());

                Ok(NodeProp::Variable(data.clone(), v.clone(), obj))
            },
        }
    }

    unsafe fn sync_children(&self, state: &mut GNodeState, gdnode: &GDNode) -> Result<(), NodeApplyError> {
        if !gdnode.has_method("set_children") {
            return Err(NodeApplyError::MissingSetChildren(self.path.clone()))
        }

        let delta = Dictionary::new();

        for (name, node) in &self.children {
            let (ns, n) = match state.children.remove(name) {
                Some((s, n)) => (Some(s), Some(n)),
                None => (None, None)
            };

            let (new_state, result) = node.apply(ns, n)
                .map_err(|e| NodeApplyError::ChildError(self.path.clone(), name.clone(), e))?;

            match result {
                NodeResult::Replace(new_node) => {
                    if let Some(n) = &n {
                        n.assume_safe().queue_free();
                    }
                    delta.insert(name.clone(), new_node);
                    state.children.insert(name.clone(), (new_state, new_node));
                },
                NodeResult::Keep => {
                    state.children.insert(name.clone(), (new_state, n.unwrap()));
                }
            }
        }

        state.children
            .drain_filter(|name, _| !self.children.contains_key(name))
            .for_each(|(name, (_, node))| {
                node.assume_safe().queue_free();
                delta.insert(name, ().to_variant());
            });

        if !delta.is_empty() {
            gdnode.call("set_children", &[delta.owned_to_variant()]);
        }

        for (_, n) in state.children.values() {
            let n = n.assume_safe();
            match n.owner() {
                Some(o) => {
                    if o.assume_safe().get_path() != gdnode.get_path() {
                        if let Some(o) = gdnode.owner() {
                            n.set_owner(o);
                        }
                    }
                },
                None => {
                    if let Some(o) = gdnode.owner() {
                        n.set_owner(o);
                    }
                },
            }
        }

        Ok(())
    }

}

#[derive(NativeClass)]
#[inherit(Reference)]
#[register_with(Self::register)]
#[user_data(::gdnative::export::user_data::MutexData<Self>)]
pub struct DispatchObject {
    pub id: Uuid,
    pub tx: Option<Arc<BroadcastEvent<Value>>>,
}

#[methods]
impl DispatchObject {
    fn new(_owner: &Reference) -> Self {
        DispatchObject {
            id: Uuid::nil(),
            tx: None
        }
    }

    #[method]
    fn send(&self, value: Variant) {
        if let Err(e) = self.tx.as_ref()
                .ok_or_else(|| "DispatchObject::tx must be set".to_boxed_error())
                .and_then(|tx| tx.invoke(&variant_to_serde_lossy(&value))) {
            tracing::error!(what = "faiiled to invoke tx event for dispatch", why = %e, who = ?self.id);
        }
    }

    pub fn handle(this: &Instance<Self, Shared>, value: &Value) {
        let i = unsafe { this.assume_safe() };

        i.base().emit_signal("on_message_received", &[
            serde_to_variant_lossy(&value)
        ]);
    }

    fn register(builder: &ClassBuilder<DispatchObject>) {
        builder
            .signal("on_message_received")
            .with_param_untyped("msg")
            .done();
    }
}

#[derive(Debug)]
struct GodotDispatch {
    id: Uuid,
    rx: Delegate<Value>,
    tx: BroadcastEvent<Value>,
}

impl GodotDispatch {
    pub fn new(obj: Instance<DispatchObject, Shared>, id: Uuid) -> Self {
        Self {
            id,
            rx: Delegate::<Value>::new({
                Box::new(move |value| {
                    DispatchObject::handle(&obj, value);
                    Ok(())
                })
            }),
            tx: BroadcastEvent::new()
        }
    }
}

impl Dispatch for GodotDispatch {
    fn send(&self, value: Value) -> BoxedResult<()> {
        self.tx.invoke(&value)
    }

    fn get_rx(&self) -> utils::DynInvokeRef<Value> {
        &self.rx
    }

    fn get_tx(&self) -> utils::SubscriptionWatchRef<Value> {
        &self.tx
    }

    fn get_id(&self) -> BoxedResult<Uuid> {
        Ok(self.id.clone())
    }
}

#[derive(NativeClass)]
#[inherit(Reference)]
#[user_data(::gdnative::export::user_data::MutexData<Self>)]
#[register_with(Self::register)]
#[derive(Derivative)]
#[derivative(Debug)]
struct GodotVariable {
    var: Option<Weak<dyn Variable>>,
    #[derivative(Debug = "ignore")]
    sub: Option<Subscription<VariableValue>>,
}

#[methods]
impl GodotVariable {
    fn new(_: &Reference) -> Self {
        Self {
            var: None,
            sub: None
        }
    }

    pub fn init(&mut self, inst: Instance<Self, Shared>, var: &Arc<dyn Variable>) -> BoxedResult<()> {
        self.var = Some(Arc::downgrade(var));
        self.sub = Some(var.on_changed().watch(Box::new(move |value| {
            unsafe {
                inst.assume_safe().base().call_deferred("changed", &[value.to_variant()]);
            }
            Ok(())
        }))?);

        Ok(())
    }

    pub fn reset(&mut self) {
        drop(self.sub.take());
    }

    #[method]
    unsafe fn set_value(&mut self, new_value: VariableValue) {
        if let Some(v) = self.var.as_ref().and_then(|v| v.upgrade()) {
            match v.set(new_value) {
                Ok(_) => {},
                Err(e) => {
                    error!(what = "error setting variable value", vid = ?v.get_id(), variable = ?v, why = %e);
                }
            }
        } else {
            error!(what = "error setting variable value", why = "variable was dropped");
        }
    }

    #[method]
    unsafe fn get_value(&self) -> Option<VariableValue> {
        self.var.as_ref()
            .and_then(|v| match v.upgrade() {
                Some(v) => Some(v),
                None => {
                    error!(what = "error fetching variable value", why = "variable was dropped");
                    None
                }
            })
            .and_then(|v| match v.get() {
                Ok(x) => Some(x),
                Err(e) => {
                    error!(what = "error fetching variable value", vid = ?v.get_id(), variable = ?v, why = %e);
                    None
                }
            })
    }

    #[method]
    unsafe fn get_value_as(&self, t: u32) -> Variant {
        match self.get_value() {
            Some(v) => match v.to_variant_typed(VariantType::from_sys(t)) {
                Ok(v) => v,
                Err(e) => {
                    error!(what = "error converting variable value to variant type", "type" = t, why = %e);
                    ().to_variant()
                }
            },
            None => ().to_variant()
        }
    }

    #[method]
    unsafe fn changed(&self, #[base] owner: &Reference, value: Variant) {
        owner.emit_signal("on_changed", &[value]);
    }

    fn register(builder: &ClassBuilder<Self>) {
        builder
            .signal("on_changed")
            .with_param_untyped("value")
            .done();
    }
}

pub fn init(handle: &InitHandle) {
    handle.add_class::<DispatchObject>();
    handle.add_class::<GodotVariable>();
}


fn serde_to_variant(v: &Value, t: &VariantType, array_types: Option<&[VariantType]>) -> Option<Variant> {
    if matches!(v, Value::Null) {
        return Some(().to_variant());
    }

    match t {
        VariantType::Nil => v.as_null().map(|_| ().to_variant()),
        VariantType::Bool => v.as_bool().map(|x| x.to_variant()),
        VariantType::I64 => v.as_i64().map(|x| x.to_variant()),
        VariantType::F64 => v.as_f64().map(|x| x.to_variant()),
        VariantType::GodotString => v.as_str().map(|x| GodotString::from_str(x).to_variant()),
        VariantType::Vector2 => serde_json::from_value::<Vector2>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Rect2 => serde_json::from_value::<Rect2>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Vector3 => serde_json::from_value::<Vector3>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Transform2D => serde_json::from_value::<Transform2D>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Plane => serde_json::from_value::<Plane>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Quat => serde_json::from_value::<Quat>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Aabb => serde_json::from_value::<Aabb>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Basis => serde_json::from_value::<Basis>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Transform => serde_json::from_value::<Transform>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Color => serde_json::from_value::<Color>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::NodePath => serde_json::from_value::<NodePath>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::ByteArray => serde_json::from_value::<ByteArray>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Int32Array => serde_json::from_value::<Int32Array>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Float32Array => serde_json::from_value::<Float32Array>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::StringArray => serde_json::from_value::<StringArray>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Vector2Array => serde_json::from_value::<Vector2Array>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::Vector3Array => serde_json::from_value::<Vector3Array>(v.clone()).ok().map(|v| v.to_variant()),
        VariantType::ColorArray => serde_json::from_value::<ColorArray>(v.clone()).ok().map(|v| v.to_variant()),

        VariantType::VariantArray => {
            if let Value::Array(a) = v && let Some(t) = array_types {
                let atype = t.get(0).expect("Expected array_types to have at least one value for t == VariantArray");
                Some(a.iter()
                    .map(|v| {
                        serde_to_variant(v, atype, if t.is_empty() { None } else { Some(&t[1..]) })
                    })
                    .collect::<Vec<_>>()
                    .to_variant()
                )
            } else {
                None
            }
        },

        VariantType::Dictionary => {
            debug!(what = "performing lossy/typeless conversion into godot Dictionary", ?v);
            Some(serde_to_variant_lossy(v))
        },

        x @ VariantType::Rid |
        x @ VariantType::Object => {
            debug!(what = "cannot convert serde value to godot value of type Rid or Object", "type" = ?x, ?v);
            None
        },
    }
}

fn serde_to_variant_lossy(v: &Value) -> Variant {
    match v {
        Value::Null => ().to_variant(),
        Value::Bool(b) => b.to_variant(),
        Value::Number(i) => match () {
            _ if i.is_f64() => i.as_f64().unwrap().to_variant(),
            _ if i.is_i64() => i.as_i64().unwrap().to_variant(),
            _ if i.is_u64() => i.as_u64().unwrap().to_variant(),
            _ => panic!()
        },
        Value::String(s) => s.to_variant(),
        Value::Array(a) => {
            a.iter().map(serde_to_variant_lossy).collect::<Vec<_>>().to_variant()
        },
        Value::Object(o) => {
            o.iter().map(|(k, v)| (k, serde_to_variant_lossy(v))).collect::<HashMap<_, _>>().to_variant()
        },
    }
}

fn variant_to_serde_lossy(v: &Variant) -> Value {
    match v.dispatch() {
        VariantDispatch::Nil => Value::Null,
        VariantDispatch::Bool(b) => serde_json::to_value(b).unwrap(),
        VariantDispatch::I64(i) => serde_json::to_value(i).unwrap(),
        VariantDispatch::F64(f) => serde_json::to_value(f).unwrap(),
        VariantDispatch::GodotString(s) => Value::String(s.to_string()),
        VariantDispatch::Vector2(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Rect2(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Vector3(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Transform2D(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Plane(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Quat(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Aabb(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Basis(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Transform(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Color(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::NodePath(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::ByteArray(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Int32Array(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Float32Array(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::StringArray(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Vector2Array(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::Vector3Array(x) => serde_json::to_value(x).unwrap(),
        VariantDispatch::ColorArray(x) => serde_json::to_value(x).unwrap(),

        VariantDispatch::Dictionary(x) => Value::Object(x.iter()
            .map(|(k, v)| (k.to_string(), variant_to_serde_lossy(&v)))
            .collect::<serde_json::Map<String, Value>>()
        ),
        VariantDispatch::VariantArray(x) => Value::Array(x.iter()
            .map(|v| variant_to_serde_lossy(&v))
            .collect::<Vec<_>>()
        ),

        VariantDispatch::Rid(_) => Value::Null,
        VariantDispatch::Object(_) => Value::Null,
    }
}

impl FromVariant for VariableValue {
    fn from_variant(variant: &Variant) -> Result<Self, FromVariantError> {
        match variant.dispatch() {
            VariantDispatch::Nil => Err(FromVariantError::InvalidNil),

            VariantDispatch::Vector2(x) => Ok(Self::Vec2([x.x as f64, x.y as f64])),
            VariantDispatch::Vector3(x) => Ok(Self::Vec3([x.x as f64, x.y as f64, x.z as f64])),
            VariantDispatch::Int32Array(x) => Ok(Self::VecFixed(x.to_vec().into_iter().map(|i| i as f64).collect())),
            VariantDispatch::Float32Array(x) => Ok(Self::VecFixed(x.to_vec().into_iter().map(|i| i as f64).collect())),

            VariantDispatch::Rect2(Rect2 { position, size }) => Ok(Self::VecFixed(vec![
                position.x as f64, position.y as f64,
                size.x as f64, size.y as f64,
            ])),
            VariantDispatch::Transform2D(Transform2D { a, b, origin }) => Ok(Self::VecFixed(vec![
                a.x as f64, a.y as f64,
                b.x as f64, b.y as f64,
                origin.x as f64, origin.y as f64,
            ])),
            VariantDispatch::Plane(Plane { normal, d }) => Ok(Self::VecFixed(vec![
                normal.x as f64, normal.y as f64, normal.x as f64,
                d as f64,
            ])),
            VariantDispatch::Quat(Quat { x, y, z, w }) => Ok(Self::VecFixed(vec![
                x as f64, y as f64,
                z as f64, w as f64,
            ])),
            VariantDispatch::Aabb(Aabb { position, size }) => Ok(Self::VecFixed(vec![
                position.x as f64, position.y as f64, position.z as f64,
                size.x as f64, size.y as f64, size.z as f64,
            ])),
            VariantDispatch::Basis(Basis { elements: [x, y, z] }) => Ok(Self::VecFixed(vec![
                x.x as f64, x.y as f64, x.z as f64,
                y.x as f64, y.y as f64, y.z as f64,
                z.x as f64, z.y as f64, z.z as f64,
            ])),
            VariantDispatch::Transform(Transform { basis: Basis { elements: [x, y, z] }, origin }) => Ok(Self::VecFixed(vec![
                x.x as f64, x.y as f64, x.z as f64,
                y.x as f64, y.y as f64, y.z as f64,
                z.x as f64, z.y as f64, z.z as f64,
                origin.x as f64, origin.y as f64, origin.z as f64,
            ])),
            VariantDispatch::Color(Color { r, g, b, a }) => Ok(Self::VecFixed(vec![
                r as f64, g as f64, b as f64, a as f64,
            ])),
    
            _ => Err(FromVariantError::custom(format!("Variant {variant} cannot be converted into a VariableValue")))
        }
    }
}

impl ToVariant for VariableValue {
    fn to_variant(&self) -> Variant {
        match self {
            VariableValue::Int(i) => i.to_variant(),
            VariableValue::Float(f) => f.to_variant(),
            VariableValue::Vec2(v) => Vector2::new(v[0] as f32, v[1] as f32).owned_to_variant(),
            VariableValue::Vec3(v) => Vector3::new(v[0] as f32, v[1] as f32, v[2] as f32).owned_to_variant(),
            VariableValue::VecFixed(v) => v.owned_to_variant(),
        }
    }
}

impl VariableValue {
    pub fn to_variant_typed(&self, t: VariantType) -> BoxedResult<Variant> {
        match (self, t) {
            (VariableValue::Int(x), VariantType::I64) => Ok(x.to_variant()),
            (VariableValue::Int(x), VariantType::F64) => Ok((*x as f64).to_variant()),
            (VariableValue::Float(x), VariantType::I64) => Ok((*x as i64).to_variant()),
            (VariableValue::Float(x), VariantType::F64) => Ok(x.to_variant()),

            (VariableValue::Vec2([x, y]), VariantType::Vector2) => {
                Ok(Vector2{ x: *x as f32, y: *y as f32 }.owned_to_variant())
            },
            (VariableValue::VecFixed(v), VariantType::Vector2) if v.len() == 2 => {
                Ok(Vector2{ x: v[0] as f32, y: v[1] as f32 }.owned_to_variant())
            },

            (VariableValue::Vec3([x, y, z]), VariantType::Vector3) => {
                Ok(Vector3{ x: *x as f32, y: *y as f32, z: *z as f32 }.owned_to_variant())
            },
            (VariableValue::VecFixed(v), VariantType::Vector3) if v.len() == 3 => {
                Ok(Vector3{ x: v[0] as f32, y: v[1] as f32, z: v[2] as f32 }.owned_to_variant())
            },

            (VariableValue::VecFixed(v), VariantType::Rect2) if v.len() == 4 => {
                Ok(Rect2 {
                    position: Vector2 { x: v[0] as f32, y: v[1] as f32 },
                    size: Vector2 { x: v[2] as f32, y: v[3] as f32 },
                }.owned_to_variant())
            },

            (VariableValue::VecFixed(v), VariantType::Transform2D) if v.len() == 6 => {
                Ok(Transform2D {
                    a: Vector2 { x: v[0] as f32, y: v[1] as f32 },
                    b: Vector2 { x: v[2] as f32, y: v[3] as f32 },
                    origin: Vector2 { x: v[4] as f32, y: v[5] as f32 }
                }.owned_to_variant())
            },

            (VariableValue::VecFixed(v), VariantType::Plane) if v.len() == 4 => {
                Ok(Plane {
                    normal: Vector3 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32 },
                    d: v[3] as f32,
                }.owned_to_variant())
            },

            (VariableValue::VecFixed(v), VariantType::Quat) if v.len() == 4 => {
                Ok(Quat {
                    x: v[0] as f32,
                    y: v[1] as f32,
                    z: v[2] as f32,
                    w: v[3] as f32,
                }.owned_to_variant())
            },

            (VariableValue::VecFixed(v), VariantType::Aabb) if v.len() == 6 => {
                Ok(Aabb {
                    position: Vector3 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32 },
                    size: Vector3 { x: v[3] as f32, y: v[4] as f32, z: v[5] as f32 },
                }.owned_to_variant())
            },

            (VariableValue::VecFixed(v), VariantType::Basis) if v.len() == 9 => {
                Ok(Basis {
                    elements: [
                        Vector3 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32 },
                        Vector3 { x: v[3] as f32, y: v[4] as f32, z: v[5] as f32 },
                        Vector3 { x: v[6] as f32, y: v[7] as f32, z: v[8] as f32 },
                    ],
                }.owned_to_variant())
            },

            (VariableValue::VecFixed(v), VariantType::Transform) if v.len() == 12 => {
                Ok(Transform {
                    basis: Basis {
                        elements: [
                            Vector3 { x: v[0] as f32, y: v[1] as f32, z: v[2] as f32 },
                            Vector3 { x: v[3] as f32, y: v[4] as f32, z: v[5] as f32 },
                            Vector3 { x: v[6] as f32, y: v[7] as f32, z: v[8] as f32 },
                        ],
                    },
                    origin: Vector3 { x: v[9] as f32, y: v[10] as f32, z: v[11] as f32 }
                }.owned_to_variant())
            },

            (VariableValue::VecFixed(v), VariantType::Color) if v.len() == 4 => {
                Ok(Color {
                    r: v[0] as f32,
                    g: v[1] as f32,
                    b: v[2] as f32,
                    a: v[3] as f32,
                }.owned_to_variant())
            },
    
            _ => {
                #[derive(thiserror::Error, Debug)]
                #[error("Cannot convert VariableValue {value:?} into variant of type {variant:?}")]
                struct InvalidVariableValue{value: VariableValue, variant: VariantType}
        
                Err(Box::new(InvalidVariableValue {
                    value: self.clone(),
                    variant: t
                }))
            }
        }
    }
}
