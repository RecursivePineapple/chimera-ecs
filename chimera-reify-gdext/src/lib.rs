#![feature(iterator_try_collect)]
#![feature(test)]
#![feature(hash_extract_if)]
#![feature(let_chains)]
#![feature(extract_if)]
#![feature(assert_matches)]
#![feature(result_flattening)]

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Weak,
    },
};

use godot::{
    global::{self, PropertyUsageFlags},
    prelude::{Node as GDNode, *},
};

use chimera_utils::*;
use itertools::Itertools;
use serde_json::Value;
use tracing::{debug, error};
use uuid::Uuid;

use chimera_core::{
    node::SceneProp,
    prelude::{Node as CNode, *},
};

pub struct RenderGDEXT;

unsafe impl ExtensionLibrary for RenderGDEXT {}

#[derive(thiserror::Error, Debug)]
pub enum NodeApplyError {
    #[error("could not find parent node {0}")]
    MissingParent(String),
    #[error("could not find scene '{0}'")]
    BadScenePath(String),
    #[error("resource '{0}' was not a PackedScene (was {1})")]
    BadResourceType(String, String),
    #[error("scene '{0}' has no root node")]
    BadSceneRoot(String),
    #[error("error setting property")]
    SetProperty(#[from] BoxedError),
    #[error("error converting value for property '{0}' to type '{1:?}' (value = '{2}'): {3}")]
    BadPropertyValue(String, PropertyType, Value, BoxedError),
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
    Dispatch(
        DispatchData,
        #[allow(dead_code)] Arc<dyn Dispatch>,
        Arc<DispatchWrapper>,
        DispatchConnection,
    ),
    Variable(
        VariableData,
        #[allow(dead_code)] Arc<dyn Variable>,
        Gd<GodotVariable>,
    ),
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
            (Self::Value(l0), SceneProp::Value(r0, _)) => l0 == r0,
            (Self::Dispatch(l0, _, _, _), SceneProp::Dispatch(_, r0)) => l0 == r0,
            (Self::Variable(l0, _, _), SceneProp::Variable(_, r0)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Debug, Default)]
struct GNodeState {
    pub scene: String,

    pub props: HashMap<String, NodeProp>,

    pub children: HashMap<String, (GNodeState, Gd<GDNode>)>,

    pub prop_cache: Option<(InstanceId, HashMap<String, Property>)>,
}

enum NodeResult {
    Keep,
    Replace(Gd<GDNode>),
}

#[derive(Debug)]
pub struct Property {
    pub name: String,
    pub prop_type: PropertyType,
    pub category: Option<String>,
    pub hint: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyType {
    Builtin(VariantType),
    Object(ClassName),
    Array(Box<PropertyType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassName {
    String(String),
    StringName(StringName),
}

impl Display for ClassName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClassName::String(s) => f.write_str(s),
            ClassName::StringName(s) => f.write_str(&s.to_string()),
        }
    }
}

fn get_properties(props: Array<Dictionary>) -> BoxedResult<HashMap<String, Property>> {
    let mut parsed = HashMap::new();

    let mut category = None;

    for i in 0..props.len() {
        let curr = props.at(i);

        let name = curr
            .get("name")
            .ok_or("could not get 'name'")?
            .try_to::<String>()
            .map_err(|_| {
                format!(
                    "error while processing property {curr:?}: could not convert 'name' to string"
                )
                .to_boxed_error()
            })?;

        let usage = curr
            .get("usage")
            .unwrap_or_default()
            .try_to::<PropertyUsageFlags>()
            .map_err(|_| format!("error while processing property {name}: could not convert 'usage' to PropertyUsageFlags").to_boxed_error())?;

        if usage == PropertyUsageFlags::CATEGORY {
            category = Some(name);
        } else {
            let (prop_type, hint) = parse_property_type(curr)
                .map_err_explain_with(|| format!("error while processing property {name}"))?;

            parsed.insert(
                name.clone(),
                Property {
                    name,
                    category: category.take(),
                    prop_type,
                    hint,
                },
            );
        }
    }

    Ok(parsed)
}

fn parse_property_type(d: Dictionary) -> BoxedResult<(PropertyType, String)> {
    tracing::debug!(what = "processing storage property", ?d);

    let ptype = d
        .get("type")
        .ok_or_else(|| "could not get 'type'")?
        .try_to::<VariantType>()
        .map_err(|_| "could not convert 'type' to VariantType".to_boxed_error())?;

    let hint_str = d
        .get("hint_string")
        .unwrap_or_else(|| String::new().to_variant())
        .try_to::<String>()
        .map_err(|_| "could not convert 'hint_string' to string".to_boxed_error())?;

    if ptype == VariantType::ARRAY {
        let (types, hint) = if let Some(idx) = hint_str.find('/') {
            hint_str.split_at(idx)
        } else {
            (hint_str.as_str(), "")
        };

        let types: Vec<PropertyType> = types
            .split(':')
            .filter(|s| !s.is_empty())
            .map(|t| parse_variant(t))
            .collect();

        fn convert(rest: &[PropertyType]) -> PropertyType {
            if rest[0] == PropertyType::Builtin(VariantType::ARRAY) {
                PropertyType::Array(Box::new(convert(&rest[1..])))
            } else {
                rest[0].clone()
            }
        }

        Ok((
            PropertyType::Array(Box::new(convert(&types[..]))),
            hint.to_owned(),
        ))
    } else if ptype == VariantType::OBJECT {
        let n = d
            .get("class_name")
            .ok_or("could not get 'class_name'")?
            .try_to::<StringName>()
            .map_err(|_| "could not convert 'class_name' to StringName".to_boxed_error())?;

        Ok((PropertyType::Object(ClassName::StringName(n)), hint_str))
    } else {
        Ok((PropertyType::Builtin(ptype), hint_str))
    }
}

fn parse_variant(t: &str) -> PropertyType {
    match t {
        "Nil" => PropertyType::Builtin(VariantType::NIL),
        "Bool" => PropertyType::Builtin(VariantType::BOOL),
        "Int" => PropertyType::Builtin(VariantType::INT),
        "Float" => PropertyType::Builtin(VariantType::FLOAT),
        "String" => PropertyType::Builtin(VariantType::STRING),
        "Vector2" => PropertyType::Builtin(VariantType::VECTOR2),
        "Vector2i" => PropertyType::Builtin(VariantType::VECTOR2I),
        "Rect2" => PropertyType::Builtin(VariantType::RECT2),
        "Rect2i" => PropertyType::Builtin(VariantType::RECT2I),
        "Vector3" => PropertyType::Builtin(VariantType::VECTOR3),
        "Vector3i" => PropertyType::Builtin(VariantType::VECTOR3I),
        "Transform2D" => PropertyType::Builtin(VariantType::TRANSFORM2D),
        "Vector4" => PropertyType::Builtin(VariantType::VECTOR4),
        "Vector4i" => PropertyType::Builtin(VariantType::VECTOR4I),
        "Plane" => PropertyType::Builtin(VariantType::PLANE),
        "Quaternion" => PropertyType::Builtin(VariantType::QUATERNION),
        "Aabb" => PropertyType::Builtin(VariantType::AABB),
        "Basis" => PropertyType::Builtin(VariantType::BASIS),
        "Transform3D" => PropertyType::Builtin(VariantType::TRANSFORM3D),
        "Projection" => PropertyType::Builtin(VariantType::PROJECTION),
        "Color" => PropertyType::Builtin(VariantType::COLOR),
        "StringName" => PropertyType::Builtin(VariantType::STRING_NAME),
        "NodePath" => PropertyType::Builtin(VariantType::NODE_PATH),
        "Rid" => PropertyType::Builtin(VariantType::RID),
        "Object" => PropertyType::Builtin(VariantType::OBJECT),
        "Callable" => PropertyType::Builtin(VariantType::CALLABLE),
        "Signal" => PropertyType::Builtin(VariantType::SIGNAL),
        "Dictionary" => PropertyType::Builtin(VariantType::DICTIONARY),
        "Array" => PropertyType::Builtin(VariantType::ARRAY),
        "PackedByteArray" => PropertyType::Builtin(VariantType::PACKED_BYTE_ARRAY),
        "PackedInt32Array" => PropertyType::Builtin(VariantType::PACKED_INT32_ARRAY),
        "PackedInt64Array" => PropertyType::Builtin(VariantType::PACKED_INT64_ARRAY),
        "PackedFloat32Array" => PropertyType::Builtin(VariantType::PACKED_FLOAT32_ARRAY),
        "PackedFloat64Array" => PropertyType::Builtin(VariantType::PACKED_FLOAT64_ARRAY),
        "PackedStringArray" => PropertyType::Builtin(VariantType::PACKED_STRING_ARRAY),
        "PackedVector2Array" => PropertyType::Builtin(VariantType::PACKED_VECTOR2_ARRAY),
        "PackedVector3Array" => PropertyType::Builtin(VariantType::PACKED_VECTOR3_ARRAY),
        "PackedColorArray" => PropertyType::Builtin(VariantType::PACKED_COLOR_ARRAY),
        other => PropertyType::Object(ClassName::String(other.to_owned())),
    }
}

pub struct MountPoint {
    node_name: String,
    gstate: Option<GNodeState>,
}

impl MountPoint {
    pub fn new(node_name: impl Into<String>) -> Self {
        MountPoint {
            node_name: node_name.into(),
            gstate: None,
        }
    }

    pub fn reset(&mut self, root: Gd<GDNode>) -> BoxedResult<()> {
        if let Some(mut n) = root.get_node_or_null(self.node_name.as_str().into()) {
            if let Some(mut p) = n.get_parent() {
                p.remove_child(n.clone());
            }

            n.queue_free();
        }

        fn reset_state(state: GNodeState) {
            for (_, (state, mut node)) in state.children {
                node.queue_free();
                reset_state(state);
            }
        }

        if let Some(state) = self.gstate.take() {
            reset_state(state);
        }

        Ok(())
    }

    pub fn redraw(&mut self, root: Gd<GDNode>, tree: &CNode) -> BoxedResult<()> {
        let mut cnode = root.get_node_or_null(self.node_name.as_str().into());

        if cnode.is_none() {
            let mut new_node = node::create(tree)?;

            let ppath =
                &self.node_name[..self.node_name.rfind('/').unwrap_or(self.node_name.len())];
            let mut parent = root
                .get_node_or_null(ppath.into())
                .ok_or_else(|| NodeApplyError::MissingParent(ppath.to_owned()))?;

            new_node.set_name(
                self.node_name[self.node_name.rfind('/').map(|i| i + 1).unwrap_or(0)..].into(),
            );
            parent.add_child(new_node.clone());

            if let Some(owner) = parent.get_owner() {
                new_node.set_owner(owner);
            }

            cnode = Some(new_node);
        }

        let (state, result) = node::apply(tree, self.gstate.take(), cnode.clone(), None)?;

        self.gstate = Some(state);

        if let NodeResult::Replace(mut new_node) = result {
            if let Some(mut r) = cnode {
                if let Some(mut p) = r.get_parent() {
                    p.remove_child(r.clone());
                }

                r.queue_free();
            }

            let ppath =
                &self.node_name[..self.node_name.rfind('/').unwrap_or(self.node_name.len())];
            let mut parent = root
                .get_node_or_null(ppath.into())
                .ok_or_else(|| NodeApplyError::MissingParent(ppath.to_owned()))?;

            new_node.set_name(
                self.node_name[self.node_name.rfind('/').map(|i| i + 1).unwrap_or(0)..].into(),
            );
            parent.add_child(new_node.clone());

            if let Some(owner) = parent.get_owner() {
                new_node.set_owner(owner);
            }
        }

        Ok(())
    }
}

pub struct ActiveMountNodeSink {
    mount: MountPoint,
    source: Option<Box<dyn PassiveNodeSource>>,
    changed: Arc<AtomicBool>,
    _sub: Option<Subscription<()>>,
}

impl ActiveMountNodeSink {
    pub fn new(node_name: String) -> Self {
        Self {
            mount: MountPoint::new(node_name),
            changed: Arc::new(AtomicBool::new(false)),
            source: None,
            _sub: None,
        }
    }

    pub fn poll(&mut self, root: Gd<GDNode>) -> BoxedResult {
        if self.changed.load(Ordering::Relaxed)
            && let Some(source) = self.source.as_ref()
        {
            match source.get_tree()? {
                Some(tree) => {
                    self.mount.redraw(root, tree.as_ref())?;
                }
                None => {
                    self.mount.reset(root)?;
                }
            }
        }

        Ok(())
    }
}

impl ActiveNodeSink for ActiveMountNodeSink {
    fn feed_from<Src: PassiveNodeSource + Sync + Send + 'static>(
        &mut self,
        source: Src,
    ) -> BoxedResult {
        let changed = self.changed.clone();
        self._sub = Some(source.on_changed().watch(Box::new(move |_| {
            changed.store(true, Ordering::Relaxed);
            Ok(())
        }))?);

        self.source = Some(Box::new(source));
        Ok(())
    }
}

pub struct PassiveMountNode {
    mount: MountPoint,
    changed: Arc<AtomicBool>,
    tree: Arc<RwLock<Option<Arc<CNode>>>>,
}

impl PassiveMountNode {
    pub fn new(node_name: String) -> Self {
        Self {
            mount: MountPoint::new(node_name),
            changed: Arc::new(AtomicBool::new(false)),
            tree: Arc::new(RwLock::new(None)),
        }
    }

    pub fn poll(&mut self, root: Gd<GDNode>) -> BoxedResult {
        if let Ok(true) =
            self.changed
                .compare_exchange(true, false, Ordering::Relaxed, Ordering::Relaxed)
        {
            match self.tree.with_immut(|t| t.clone())? {
                Some(tree) => {
                    self.mount.redraw(root, tree.as_ref())?;
                }
                None => {
                    self.mount.reset(root)?;
                }
            }
        }

        Ok(())
    }

    pub fn get_sink(&self) -> PassiveMountNodeSink {
        PassiveMountNodeSink {
            changed: self.changed.clone(),
            tree: self.tree.clone(),
        }
    }
}

pub struct PassiveMountNodeSink {
    changed: Arc<AtomicBool>,
    tree: Arc<RwLock<Option<Arc<CNode>>>>,
}

impl PassiveNodeSink for PassiveMountNodeSink {
    fn push(&self, tree: Option<Arc<CNode>>) -> BoxedResult {
        self.tree.with_mut(|t| {
            *t = tree;
        })?;

        self.changed.store(true, Ordering::Relaxed);

        Ok(())
    }
}

mod node {
    use godot::classes::ResourceLoader;

    use super::*;

    pub fn apply(
        tree: &CNode,
        prev: Option<GNodeState>,
        n: Option<Gd<GDNode>>,
        name: Option<&str>,
    ) -> BoxedResult<(GNodeState, NodeResult)> {
        let mut state = prev.unwrap_or_default();

        let prev_iid = n
            .as_ref()
            .and_then(|n| n.is_instance_valid().then(|| n.instance_id()));

        let mut node = if tree.scene == state.scene {
            match n {
                Some(n) => n,
                None => create(tree)?,
            }
        } else {
            state.scene = tree.scene.clone();
            create(tree)?
        };

        if let Some(name) = name
            && name != node.get_name().to_string()
        {
            node.set_name(name.into());
        }

        sync_props(tree, &mut state, &mut node)?;

        if node.has_method("set_children".into()) {
            sync_children(tree, &mut state, &mut node)?;
        }

        Ok((
            state,
            if Some(node.instance_id()) == prev_iid {
                NodeResult::Keep
            } else {
                NodeResult::Replace(node)
            },
        ))
    }

    pub fn create(node: &CNode) -> Result<Gd<GDNode>, NodeApplyError> {
        let mut loader = ResourceLoader::singleton();

        let scene = loader
            .load(node.scene.as_str().into())
            .ok_or_else(|| NodeApplyError::BadScenePath(node.scene.clone()))?;

        let ps = scene.try_cast::<PackedScene>().map_err(|actual| {
            NodeApplyError::BadResourceType(node.scene.clone(), format!("{actual:?}"))
        })?;

        let n = ps
            .instantiate()
            .ok_or_else(|| NodeApplyError::BadSceneRoot(node.scene.clone()))?;

        Ok(n)
    }

    pub fn sync_props(
        tree: &CNode,
        state: &mut GNodeState,
        node: &mut Gd<GDNode>,
    ) -> BoxedResult<()> {
        for (name, delta) in delta_hash_maps2(&state.props, &tree.props) {
            match delta {
                HashMapDeltaChange::Left(_) => {
                    let old_value = state.props.remove(&name).unwrap();
                    remove_prop(node, &name, old_value)?;
                }
                HashMapDeltaChange::Changed(_, _) => {
                    let old_value = state.props.remove(&name).unwrap();
                    let new_value = tree.props.get(&name).unwrap();

                    if !matches!(old_value, NodeProp::Value(_)) {
                        remove_prop(node, &name, old_value)?;
                    }

                    let p = setup_prop(state, node, &name, new_value)?;

                    state.props.insert(name.clone(), p);
                }
                HashMapDeltaChange::Right(_) => {
                    let new_value = tree.props.get(&name).unwrap();

                    let p = setup_prop(state, node, &name, new_value)?;

                    state.props.insert(name.clone(), p);
                }
            }
        }

        Ok(())
    }

    pub fn remove_prop(
        node: &mut Gd<GDNode>,
        name: &String,
        old_value: NodeProp,
    ) -> BoxedResult<()> {
        match old_value {
            NodeProp::Value(_) => {
                node.set(name.into(), &Variant::nil());
                Ok(())
            }
            NodeProp::Dispatch(_, _, gd, conn) => {
                node.set(name.into(), &Variant::nil());

                drop(conn);
                drop(gd);

                Ok(())
            }
            NodeProp::Variable(_, _, gv) => {
                node.set(name.into(), &Variant::nil());

                drop(gv);

                Ok(())
            }
        }
    }

    pub fn setup_prop(
        state: &mut GNodeState,
        node: &mut Gd<GDNode>,
        name: &String,
        new_value: &SceneProp,
    ) -> BoxedResult<NodeProp> {
        match new_value {
            SceneProp::Value(value, _) => {
                let node_props = if let Some((nid, props)) = &state.prop_cache
                    && nid == &node.instance_id()
                {
                    props
                } else {
                    state.prop_cache = Some((
                        node.instance_id(),
                        get_properties(node.get_property_list())
                            .map_err_explain("could not get object properties")?,
                    ));
                    &state.prop_cache.as_ref().unwrap().1
                };

                let v = match node_props.get(name) {
                    Some(property) => {
                        serde_to_variant(value, &property.prop_type).map_err(|e| {
                            NodeApplyError::BadPropertyValue(
                                name.clone(),
                                property.prop_type.clone(),
                                value.clone(),
                                e,
                            )
                        })?
                    }
                    None => {
                        tracing::warn!(
                            what = "using untyped (lossy) conversion for value property because the property could not be found: this will likely not work",
                            scene = state.scene,
                            property = name,
                            value = ?value,
                        );
                        serde_to_variant_lossy(value)
                    }
                };

                node.set(name.into(), &v);

                Ok(NodeProp::Value(value.clone()))
            }
            SceneProp::Dispatch(d, data) => {
                let (gd, obj) = DispatchWrapper::new(data.id.clone());

                let conn = DispatchConnection::connect(
                    d,
                    &gd,
                    data.has_tx_permissions(None),
                    data.has_rx_permissions(None),
                )?;

                node.set(name.into(), &obj.to_variant());

                Ok(NodeProp::Dispatch(data.clone(), d.clone(), gd, conn))
            }
            SceneProp::Variable(v, data) => {
                let dir = data.get_direction(None).ok_or_else(|| {
                    format!("could not get direction for variable ").to_boxed_error()
                })?;

                let gv = GodotVariable::new(v.clone(), dir).map_err_explain_with(|| {
                    format!("could not create GodotVariable for variable {}", data.id)
                })?;

                node.set(name.into(), &gv.to_variant());

                Ok(NodeProp::Variable(data.clone(), v.clone(), gv))
            }
        }
    }

    pub fn sync_children(
        tree: &CNode,
        state: &mut GNodeState,
        node: &mut Gd<GDNode>,
    ) -> Result<(), NodeApplyError> {
        if !node.has_method("set_children".into()) {
            return Err(NodeApplyError::MissingSetChildren(tree.scene.clone()));
        }

        let mut delta = Dictionary::new();

        for (name, child) in &tree.children {
            let (ns, n) = match state.children.remove(name) {
                Some((s, n)) => (Some(s), Some(n)),
                None => (None, None),
            };

            let (new_state, result) = apply(child, ns, n.clone(), Some(name))
                .map_err(|e| NodeApplyError::ChildError(tree.scene.clone(), name.clone(), e))?;

            match result {
                NodeResult::Replace(new_node) => {
                    if let Some(mut n) = n {
                        n.queue_free();
                    }
                    delta.set(name.clone(), new_node.clone());
                    state.children.insert(name.clone(), (new_state, new_node));
                }
                NodeResult::Keep => {
                    state.children.insert(name.clone(), (new_state, n.unwrap()));
                }
            }
        }

        state
            .children
            .extract_if(|name, _| !tree.children.contains_key(name))
            .for_each(|(name, (_, mut child))| {
                child.queue_free();
                delta.set(name, ().to_variant());
            });

        if !delta.is_empty() {
            node.call("set_children".into(), &[delta.to_variant()]);
        }

        for (_, child) in state.children.values_mut() {
            match child.get_owner() {
                Some(o) => {
                    if o.get_path().to_string() != node.get_path().to_string() {
                        if let Some(o) = node.get_owner() {
                            child.set_owner(o);
                        }
                    }
                }
                None => {
                    if let Some(o) = node.get_owner() {
                        child.set_owner(o);
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
struct DispatchWrapper {
    id: Uuid,
    rx: Delegate<DispatchMessage>,
    tx: BroadcastEvent<DispatchMessage>,
}

impl DispatchWrapper {
    pub fn new(id: Uuid) -> (Arc<Self>, Gd<GodotDispatch>) {
        let gd = Gd::from_object(GodotDispatch::new());

        let gdid = gd.instance_id();

        let tx = gd.bind().tx.clone();

        let s = Arc::new(Self {
            id,
            rx: Delegate::new(Box::new(move |msg| {
                let msg = if msg.message.is_serializable() {
                    msg.message
                        .clone()
                        .into_value()
                        .map_err_explain("could not serialize message for godot dispatch")?
                } else {
                    return Err(format!(
                        "could not send non-serializable message to godot dispatch: {msg:?}"
                    )
                    .to_boxed_error());
                };

                if let Ok(gd) = <Gd<GodotDispatch>>::try_from_instance_id(gdid.clone()) {
                    let err = gd.upcast::<RefCounted>().emit_signal(
                        "on_message_received".into(),
                        &[serde_to_variant_lossy(&msg)],
                    );

                    if err != global::Error::OK {
                        return Err(
                            format!("error sending message to dispatch: {err:?}").to_boxed_error()
                        );
                    }
                }

                Ok(())
            })),
            tx,
        });

        (s, gd)
    }
}

impl Dispatch for DispatchWrapper {
    unsafe fn send(&self, msg: DispatchMessage) -> BoxedResult<()> {
        self.tx.invoke(&msg)
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
}

#[derive(GodotClass)]
#[class(base=RefCounted, no_init)]
struct GodotDispatch {
    tx: BroadcastEvent<DispatchMessage>,
}

#[godot_api]
impl GodotDispatch {
    fn new() -> Self {
        Self {
            tx: BroadcastEvent::new(),
        }
    }

    #[signal]
    fn on_message_received(msg: Variant);

    #[func]
    fn send(&self, value: Variant) {
        if let Err(e) = self.tx.invoke(&DispatchMessage {
            message: Payload::Value(variant_to_serde_lossy(&value)),
        }) {
            tracing::error!(what = "failed to invoke tx event for dispatch", why = %e, ?value);
        }
    }
}

#[derive(GodotClass)]
#[class(base = RefCounted, no_init)]
struct GodotVariable {
    var: Weak<dyn Variable>,
    sub: Option<Subscription<Value>>,
    dir: VariableDirection,
}

#[godot_api]
impl GodotVariable {
    #[signal]
    fn changed(var: Gd<GodotVariable>);

    fn new(var: Arc<dyn Variable>, dir: VariableDirection) -> BoxedResult<Gd<Self>> {
        let mut gd = Gd::from_object(Self {
            var: Arc::downgrade(&var),
            sub: None,
            dir: dir.clone(),
        });

        let iid = gd.clone().upcast::<RefCounted>().instance_id();

        gd.bind_mut().sub = Some(var.on_changed().watch(Box::new(move |_| {
            if let Ok(mut base) = <Gd<RefCounted>>::try_from_instance_id(iid) {
                let var = base.to_variant();
                base.call_deferred("emit_signal".into(), &["changed".to_variant(), var]);
            }
            Ok(())
        }))?);

        Ok(gd)
    }

    #[func]
    fn set_value(&self, new_value: Variant) {
        if self.dir != VariableDirection::Write {
            error!(what = "cannot write to read-only variable", variable = ?self.var.upgrade().map(|v| v.get_id()), ?new_value);
            return;
        }

        if let Some(v) = self.var.upgrade() {
            if let Err(e) = v.set(variant_to_serde_lossy(&new_value)) {
                error!(what = "error setting variable value", vid = ?v.get_id(), variable = ?v, why = %e);
            }
        } else {
            error!(
                what = "error setting variable value",
                why = "variable was dropped"
            );
        }
    }

    #[func]
    fn get_value(&self) -> Variant {
        let var = match self.var.upgrade() {
            Some(v) => v,
            None => {
                error!(
                    what = "cannot read from disconnected variable",
                    why = "variable was dropped"
                );
                return Variant::nil();
            }
        };

        match var.get() {
            Ok(x) => serde_to_variant_lossy(&x),
            Err(e) => {
                error!(what = "error fetching variable value", vid = ?var.get_id(), variable = ?var, why = %e);
                Variant::nil()
            }
        }
    }

    #[func]
    fn get_value_as(&self, t: u32) -> Variant {
        let var = match self.var.upgrade() {
            Some(v) => v,
            None => {
                error!(
                    what = "cannot read from disconnected variable",
                    why = "variable was dropped"
                );
                return Variant::nil();
            }
        };

        let val = match var.get() {
            Ok(x) => x,
            Err(e) => {
                error!(what = "error fetching variable value", vid = ?var.get_id(), variable = ?var, why = %e);
                return Variant::nil();
            }
        };

        match serde_to_variant(&val, &PropertyType::Builtin(VariantType::from_sys(t))) {
            Ok(v) => v,
            Err(e) => {
                error!(
                    what = "error converting variable value to Variant",
                    variable = ?self.var,
                    value = ?val,
                    variant_type = ?VariantType::from_sys(t),
                    why = e
                );
                Variant::nil()
            }
        }
    }
}

fn serde_to_variant(v: &Value, t: &PropertyType) -> BoxedResult<Variant> {
    if matches!(v, Value::Null) {
        return Ok(().to_variant());
    }

    macro_rules! convert {
        ($type:ident) => {{
            serde_json::from_value::<$type>(v.clone())
                .map(|x| x.to_variant())
                .map_err_boxed()
        }};
    }

    match t {
        PropertyType::Builtin(VariantType::NIL) => v
            .as_null()
            .map(|_| Variant::nil())
            .ok_or_else(|| "could not convert value to VariantType::Nil".to_boxed_error()),
        PropertyType::Builtin(VariantType::BOOL) => v
            .as_bool()
            .map(|x| x.to_variant())
            .ok_or_else(|| "could not convert value to VariantType::Bool".to_boxed_error()),
        PropertyType::Builtin(VariantType::INT) => v
            .as_i64()
            .map(|x| x.to_variant())
            .ok_or_else(|| "could not convert value to VariantType::Int".to_boxed_error()),
        PropertyType::Builtin(VariantType::FLOAT) => v
            .as_f64()
            .map(|x| x.to_variant())
            .ok_or_else(|| "could not convert value to VariantType::Float".to_boxed_error()),
        PropertyType::Builtin(VariantType::STRING) => convert!(GString),
        PropertyType::Builtin(VariantType::STRING_NAME) => convert!(StringName),
        PropertyType::Builtin(VariantType::VECTOR2) => convert!(Vector2),
        PropertyType::Builtin(VariantType::VECTOR2I) => convert!(Vector2i),
        PropertyType::Builtin(VariantType::VECTOR3) => convert!(Vector3),
        PropertyType::Builtin(VariantType::VECTOR3I) => convert!(Vector3i),
        PropertyType::Builtin(VariantType::VECTOR4) => convert!(Vector4),
        PropertyType::Builtin(VariantType::VECTOR4I) => convert!(Vector4i),
        PropertyType::Builtin(VariantType::RECT2) => convert!(Rect2),
        PropertyType::Builtin(VariantType::RECT2I) => convert!(Rect2i),
        PropertyType::Builtin(VariantType::PLANE) => convert!(Plane),
        PropertyType::Builtin(VariantType::QUATERNION) => convert!(Quaternion),
        PropertyType::Builtin(VariantType::AABB) => convert!(Aabb),
        PropertyType::Builtin(VariantType::BASIS) => convert!(Basis),
        PropertyType::Builtin(VariantType::TRANSFORM2D) => convert!(Transform2D),
        PropertyType::Builtin(VariantType::TRANSFORM3D) => convert!(Transform3D),
        PropertyType::Builtin(VariantType::PROJECTION) => convert!(Projection),
        PropertyType::Builtin(VariantType::COLOR) => convert!(Color),
        PropertyType::Builtin(VariantType::NODE_PATH) => convert!(NodePath),
        PropertyType::Builtin(VariantType::PACKED_BYTE_ARRAY) => convert!(PackedByteArray),
        PropertyType::Builtin(VariantType::PACKED_INT32_ARRAY) => convert!(PackedInt32Array),
        PropertyType::Builtin(VariantType::PACKED_FLOAT32_ARRAY) => convert!(PackedFloat32Array),
        PropertyType::Builtin(VariantType::PACKED_STRING_ARRAY) => convert!(PackedStringArray),
        PropertyType::Builtin(VariantType::PACKED_VECTOR2_ARRAY) => convert!(PackedVector2Array),
        PropertyType::Builtin(VariantType::PACKED_VECTOR3_ARRAY) => convert!(PackedVector3Array),
        PropertyType::Builtin(VariantType::PACKED_COLOR_ARRAY) => convert!(PackedColorArray),
        PropertyType::Builtin(VariantType::PACKED_INT64_ARRAY) => convert!(PackedInt64Array),
        PropertyType::Builtin(VariantType::PACKED_FLOAT64_ARRAY) => convert!(PackedFloat64Array),

        PropertyType::Array(inner) => {
            if let Value::Array(a) = v {
                a.iter()
                    .map(|v| serde_to_variant(v, inner))
                    .try_collect()
                    .map(|a: Array<Variant>| a.to_variant())
            } else {
                Err(format!("cannot convert non-array value into Array").to_boxed_error())
            }
        }

        PropertyType::Builtin(VariantType::ARRAY) => {
            if v.is_array() {
                debug!(
                    what = "performing lossy/typeless conversion into godot Array",
                    value = ?v,
                );
                Ok(serde_to_variant_lossy(v))
            } else {
                error!(what = "cannot convert non-object value into godot Dictionary", value = ?v);
                Err(
                    format!("cannot convert non-object value into godot Dictionary")
                        .to_boxed_error(),
                )
            }
        }

        PropertyType::Builtin(VariantType::DICTIONARY) => {
            if v.is_object() {
                debug!(
                    what = "performing lossy/typeless conversion into godot Dictionary",
                    value = ?v,
                );
                Ok(serde_to_variant_lossy(v))
            } else {
                error!(what = "cannot convert non-object value into godot Dictionary", value = ?v);
                Err(
                    format!("cannot convert non-object value into godot Dictionary")
                        .to_boxed_error(),
                )
            }
        }

        property_type @ PropertyType::Builtin(
            VariantType::RID | VariantType::OBJECT | VariantType::CALLABLE | VariantType::SIGNAL,
        ) => {
            debug!(
                what = "cannot convert serde value to variant",
                ?property_type,
                ?v
            );
            Err(
                format!("cannot convert node property into variant of type {property_type:?}")
                    .to_boxed_error(),
            )
        }

        PropertyType::Object(class_name) => {
            debug!(
                what = "cannot convert serde value to object",
                ?class_name,
                ?v
            );
            Err(
                format!("cannot convert node property into object of type {class_name}")
                    .to_boxed_error(),
            )
        }

        PropertyType::Builtin(other) => panic!("unknown variant type: {other:?}"),
    }
}

fn serde_to_variant_lossy(v: &Value) -> Variant {
    match v {
        Value::Null => ().to_variant(),
        Value::Bool(b) => b.to_variant(),
        Value::Number(i) => match () {
            _ if i.is_f64() => i.as_f64().unwrap().to_variant(),
            _ if i.is_i64() => i.as_i64().unwrap().to_variant(),
            _ if i.is_u64() => (i.as_u64().unwrap() as i64).to_variant(),
            _ => panic!(),
        },
        Value::String(s) => s.to_variant(),
        Value::Array(a) => a
            .iter()
            .map(serde_to_variant_lossy)
            .collect::<VariantArray>()
            .to_variant(),
        Value::Object(o) => o
            .iter()
            .map(|(k, v)| (k.to_variant(), serde_to_variant_lossy(v)))
            .collect::<Dictionary>()
            .to_variant(),
    }
}

fn variant_to_serde_lossy(v: &Variant) -> Value {
    match v.get_type() {
        VariantType::NIL => Value::Null,
        VariantType::BOOL => Value::Bool(v.to::<bool>()),
        VariantType::INT => Value::Number(serde_json::Number::from(v.to::<i64>())),
        VariantType::FLOAT => serde_json::Number::from_f64(v.to::<f64>())
            .map(Value::Number)
            .unwrap_or_default(),
        VariantType::STRING => serde_json::to_value(v.to::<GString>()).unwrap(),
        VariantType::STRING_NAME => serde_json::to_value(v.to::<StringName>()).unwrap(),
        VariantType::VECTOR2 => serde_json::to_value(v.to::<Vector2>()).unwrap(),
        VariantType::VECTOR2I => serde_json::to_value(v.to::<Vector2i>()).unwrap(),
        VariantType::VECTOR3 => serde_json::to_value(v.to::<Vector3>()).unwrap(),
        VariantType::VECTOR3I => serde_json::to_value(v.to::<Vector3i>()).unwrap(),
        VariantType::VECTOR4 => serde_json::to_value(v.to::<Vector4>()).unwrap(),
        VariantType::VECTOR4I => serde_json::to_value(v.to::<Vector4i>()).unwrap(),
        VariantType::RECT2 => serde_json::to_value(v.to::<Rect2>()).unwrap(),
        VariantType::RECT2I => serde_json::to_value(v.to::<Rect2i>()).unwrap(),
        VariantType::PLANE => serde_json::to_value(v.to::<Plane>()).unwrap(),
        VariantType::QUATERNION => serde_json::to_value(v.to::<Quaternion>()).unwrap(),
        VariantType::AABB => serde_json::to_value(v.to::<Aabb>()).unwrap(),
        VariantType::BASIS => serde_json::to_value(v.to::<Basis>()).unwrap(),
        VariantType::TRANSFORM2D => serde_json::to_value(v.to::<Transform2D>()).unwrap(),
        VariantType::TRANSFORM3D => serde_json::to_value(v.to::<Transform3D>()).unwrap(),
        VariantType::PROJECTION => serde_json::to_value(v.to::<Projection>()).unwrap(),
        VariantType::COLOR => serde_json::to_value(v.to::<Color>()).unwrap(),
        VariantType::NODE_PATH => serde_json::to_value(v.to::<NodePath>()).unwrap(),
        VariantType::PACKED_BYTE_ARRAY => serde_json::to_value(v.to::<PackedByteArray>()).unwrap(),
        VariantType::PACKED_INT32_ARRAY => {
            serde_json::to_value(v.to::<PackedInt32Array>()).unwrap()
        }
        VariantType::PACKED_FLOAT32_ARRAY => {
            serde_json::to_value(v.to::<PackedFloat32Array>()).unwrap()
        }
        VariantType::PACKED_STRING_ARRAY => {
            serde_json::to_value(v.to::<PackedStringArray>()).unwrap()
        }
        VariantType::PACKED_VECTOR2_ARRAY => {
            serde_json::to_value(v.to::<PackedVector2Array>()).unwrap()
        }
        VariantType::PACKED_VECTOR3_ARRAY => {
            serde_json::to_value(v.to::<PackedVector3Array>()).unwrap()
        }
        VariantType::PACKED_COLOR_ARRAY => {
            serde_json::to_value(v.to::<PackedColorArray>()).unwrap()
        }
        VariantType::PACKED_INT64_ARRAY => {
            serde_json::to_value(v.to::<PackedInt64Array>()).unwrap()
        }
        VariantType::PACKED_FLOAT64_ARRAY => {
            serde_json::to_value(v.to::<PackedFloat64Array>()).unwrap()
        }

        VariantType::ARRAY => {
            let v = v
                .to::<Array<Variant>>()
                .iter_shared()
                .map(|v| variant_to_serde_lossy(&v))
                .collect::<Vec<Value>>();

            Value::Array(v)
        }

        VariantType::DICTIONARY => {
            debug!(
                what = "performing lossy/typeless conversion from godot Dictionary",
                ?v
            );

            let d = v
                .to::<Dictionary>()
                .iter_shared()
                .map(|(k, v)| match variant_to_serde_lossy(&k) {
                    Value::String(s) => (s, variant_to_serde_lossy(&v)),
                    other => (other.to_string(), variant_to_serde_lossy(&v)),
                })
                .collect::<serde_json::Map<String, Value>>();

            Value::Object(d)
        }

        x @ VariantType::RID
        | x @ VariantType::OBJECT
        | x @ VariantType::CALLABLE
        | x @ VariantType::SIGNAL => {
            debug!(what = "cannot convert variant to serde value", "type" = ?x, ?v);
            Value::Null
        }
        other => panic!("unknown variant type: {other:?}"),
    }
}
