use std::sync::{Arc, Weak};

use chimera_core::permissions::PermissionSet;
use chimera_core::test_utils::DummyDispatch;
use chimera_utils::*;
use serde_json::json;

use super::*;

#[allow(dead_code)]
struct DummyFactory {
    pub vars: Arc<Mutex<Vec<Uuid>>>,
    pub dispatches: Arc<Mutex<Vec<Uuid>>>,
}

impl DummyFactory {
    pub fn new() -> Self {
        Self {
            vars: Arc::new(Mutex::new(Vec::new())),
            dispatches: Arc::new(Mutex::new(Vec::new())),
        }
    }
}

impl TransportStateFactory for DummyFactory {
    fn get_variable(&self, v: &Uuid, _: bool, _: bool, _: Value) -> BoxedResult<Arc<dyn Variable>> {
        #[derive(Debug)]
        struct DummyVar(Weak<Mutex<Vec<Uuid>>>, Uuid);

        impl Drop for DummyVar {
            fn drop(&mut self) {
                if let Some(f) = self.0.upgrade() {
                    let mut m = f.lock().unwrap();
                    if let Some(i) = m.iter().position(|x| x == &self.1) {
                        m.remove(i);
                    }
                }
            }
        }

        impl Variable for DummyVar {
            fn get(&self) -> BoxedResult<Value> {
                unimplemented!()
            }
            fn set(&self, _: Value) -> BoxedResult<()> {
                unimplemented!()
            }
            fn on_changed(&self) -> &dyn Watchable<Value, WatchGuard = Subscription<Value>> {
                unimplemented!()
            }
            fn get_id(&self) -> Uuid {
                self.1.clone()
            }
        }

        Ok(Arc::new(DummyVar(Arc::downgrade(&self.vars), v.clone())))
    }

    fn get_dispatch(&self, _: &Uuid, _: bool, _: bool) -> BoxedResult<Arc<dyn Dispatch>> {
        unimplemented!()
    }
}

#[derive(Debug)]
struct DummyVar(Uuid);

impl Variable for DummyVar {
    fn get(&self) -> BoxedResult<Value> {
        unimplemented!()
    }
    fn set(&self, _: Value) -> BoxedResult<()> {
        unimplemented!()
    }

    fn on_changed(&self) -> &dyn Watchable<Value, WatchGuard = Subscription<Value>> {
        unimplemented!()
    }

    fn get_id(&self) -> Uuid {
        self.0.clone()
    }
}

impl PartialEq<Self> for SubDiff {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ConnectDispatch { d: l0, .. }, Self::ConnectDispatch { d: r0, .. }) => {
                l0.get_id().unwrap() == r0.get_id().unwrap()
            }
            (Self::DisconnectDispatch(l0), Self::DisconnectDispatch(r0)) => {
                l0.get_id().unwrap() == r0.get_id().unwrap()
            }
            (Self::ConnectVariable { v: l0, .. }, Self::ConnectVariable { v: r0, .. }) => {
                l0.get_id() == r0.get_id()
            }
            (Self::DisconnectVariable(l0), Self::DisconnectVariable(r0)) => {
                l0.get_id() == r0.get_id()
            }
            _ => false,
        }
    }
}

#[test]
fn diff_basic() {
    let mut differ = NodeDiffer::new();
    differ.ident = vec![ClientId::Id(Uuid::new_v4())];

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    y => "world".as_value()
                }),
                meta: str_hash_map!({
                    x => json!("hello")
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Replace {
            data: NodeData {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    "y" => ScenePropData::Value(json!("world"))
                }),
                meta: str_hash_map!({
                    "x" => json!("hello")
                })
            },
            children: HashMap::new()
        }
    );
    assert_eq!(sub, vec![]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    y => "world".as_value()
                }),
                meta: str_hash_map!({
                    x => json!("hello")
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(diff, NodeDiff::Same);
    assert_eq!(sub, vec![]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    y => "bob".as_value()
                }),
                meta: str_hash_map!({
                    x => json!("hello")
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    y => PatchOperation::Insert(ScenePropData::Value(json!("bob")))
                })),
                meta: None
            }),
            children: HashMap::new()
        }
    );
    assert_eq!(sub, vec![]);
}

#[test]
fn diff_visibility() {
    let id = ClientId::Id(Uuid::new_v4());
    let id2 = ClientId::Id(Uuid::new_v4());

    let mut differ = NodeDiffer::new();
    differ.ident = vec![id.clone()];

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Replace {
            data: NodeData {
                scene: "scene".to_owned(),
                props: str_hash_map!({}),
                meta: str_hash_map!({})
            },
            children: HashMap::new()
        }
    );
    assert_eq!(sub, vec![]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                visibility: Some(PermissionSet::allow_only(id2.clone())),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(diff, NodeDiff::Delete);
    assert_eq!(sub, vec![]);

    dbg!(&differ);
    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                visibility: Some(PermissionSet::allow_only(id.clone())),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Replace {
            data: NodeData {
                scene: "scene".to_owned(),
                props: str_hash_map!({}),
                meta: str_hash_map!({})
            },
            children: HashMap::new()
        }
    );
    assert_eq!(sub, vec![]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(diff, NodeDiff::Same);
    assert_eq!(sub, vec![]);
}

#[test]
fn diff_children() {
    let mut differ = NodeDiffer::new();
    differ.ident = vec![ClientId::Id(Uuid::new_v4())];

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Replace {
            data: NodeData {
                scene: "scene".to_owned(),
                props: str_hash_map!({}),
                meta: str_hash_map!({})
            },
            children: HashMap::new()
        }
    );
    assert_eq!(sub, vec![]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                children: str_hash_map!({
                    first => Node {
                        scene: "first".to_owned(),
                        ..Default::default()
                    }
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: None,
            children: str_hash_map!({
                first => NodeDiff::Replace {
                    data: NodeData {
                        scene: "first".to_owned(),
                        props: str_hash_map!({ }),
                        meta: str_hash_map!({ })
                    },
                    children: HashMap::new()
                }
            })
        }
    );
    assert_eq!(sub, vec![]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                children: str_hash_map!({
                    first => Node {
                        scene: "first".to_owned(),
                        ..Default::default()
                    }
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(diff, NodeDiff::Same);
    assert_eq!(sub, vec![]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                children: str_hash_map!({
                    first => Node {
                        scene: "second".to_owned(),
                        ..Default::default()
                    }
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: None,
            children: str_hash_map!({
                first => NodeDiff::Merge {
                    data: Some(NodeDataPatch {
                        scene: Some("second".to_owned()),
                        props: None,
                        meta: None
                    }),
                    children: HashMap::new()
                }
            })
        }
    );
    assert_eq!(sub, vec![]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: None,
            children: str_hash_map!({
                first => NodeDiff::Delete
            })
        }
    );
    assert_eq!(sub, vec![]);
}

#[test]
fn patch_basic() {
    let transport = DummyFactory::new();

    let mut patcher = NodePatcher::new();

    assert_eq!(patcher.get_tree(), None);

    let tree = patcher
        .push_node_diff(
            &transport,
            NodeDiff::Replace {
                data: NodeData {
                    scene: "scene".to_owned(),
                    props: str_hash_map!({}),
                    meta: str_hash_map!({}),
                },
                children: str_hash_map!({}),
            },
            &HashMap::new(),
        )
        .unwrap()
        .unwrap()
        .clone();

    fn scene(name: &'static str) -> Node {
        Node {
            scene: name.to_owned(),
            ..Default::default()
        }
    }

    assert_eq!(tree, scene("scene"));

    let tree = patcher
        .push_node_diff(&transport, NodeDiff::Same, &HashMap::new())
        .unwrap()
        .unwrap()
        .clone();

    assert_eq!(tree, scene("scene"));

    let tree = patcher
        .push_node_diff(
            &transport,
            NodeDiff::Merge {
                data: None,
                children: str_hash_map!({
                    first => NodeDiff::Replace {
                        data: NodeData {
                            scene: "scene2".to_owned(),
                            props: str_hash_map!({ }),
                            meta: str_hash_map!({ })
                        },
                        children: str_hash_map!({})
                    }
                }),
            },
            &HashMap::new(),
        )
        .unwrap()
        .unwrap()
        .clone();

    assert_eq!(
        tree,
        Node {
            scene: "scene".to_owned(),
            children: str_hash_map!({
                first => Node {
                    scene: "scene2".to_owned(),
                    ..Default::default()
                }
            }),
            ..Default::default()
        }
    );

    let tree = patcher
        .push_node_diff(
            &transport,
            NodeDiff::Merge {
                data: None,
                children: str_hash_map!({}),
            },
            &HashMap::new(),
        )
        .unwrap()
        .unwrap()
        .clone();

    assert_eq!(
        tree,
        Node {
            scene: "scene".to_owned(),
            children: str_hash_map!({
                first => Node {
                    scene: "scene2".to_owned(),
                    ..Default::default()
                }
            }),
            ..Default::default()
        }
    );

    let tree = patcher
        .push_node_diff(
            &transport,
            NodeDiff::Merge {
                data: None,
                children: str_hash_map!({
                    first => NodeDiff::Delete
                }),
            },
            &HashMap::new(),
        )
        .unwrap()
        .unwrap()
        .clone();

    assert_eq!(tree, scene("scene"));

    let tree = patcher
        .push_node_diff(&transport, NodeDiff::Delete, &HashMap::new())
        .unwrap();

    assert_eq!(tree, None);
}

#[test]
fn diff_patch() {
    let factory = DummyFactory::new();

    let mut differ = NodeDiffer::new();
    let mut patcher = NodePatcher::new();
    differ.ident = vec![ClientId::Id(Uuid::new_v4())];

    fn do_thing(
        factory: &DummyFactory,
        differ: &mut NodeDiffer,
        patcher: &mut NodePatcher,
        node: &Node,
    ) -> Node {
        let (diff, subs) = differ.push_node_tree(Some(node)).unwrap();
        let _ = subs;
        patcher
            .push_node_diff(factory, diff, &HashMap::new())
            .unwrap()
            .cloned()
            .unwrap()
    }

    let node = Node {
        scene: "scene".to_owned(),
        ..Default::default()
    };

    assert_eq!(do_thing(&factory, &mut differ, &mut patcher, &node), node);

    let node = Node {
        scene: "scene".to_owned(),
        children: str_hash_map!({
            first => Node {
                scene: "first".to_owned(),
                ..Default::default()
            }
        }),
        ..Default::default()
    };

    assert_eq!(do_thing(&factory, &mut differ, &mut patcher, &node), node);

    let node = Node {
        scene: "scene".to_owned(),
        children: str_hash_map!({
            first => Node {
                scene: "first".to_owned(),
                ..Default::default()
            }
        }),
        ..Default::default()
    };

    assert_eq!(do_thing(&factory, &mut differ, &mut patcher, &node), node);

    let node = Node {
        scene: "scene".to_owned(),
        children: str_hash_map!({
            first => Node {
                scene: "first".to_owned(),
                ..Default::default()
            }
        }),
        ..Default::default()
    };

    assert_eq!(do_thing(&factory, &mut differ, &mut patcher, &node), node);

    let node = Node {
        scene: "scene".to_owned(),
        children: str_hash_map!({
            first => Node {
                scene: "second".to_owned(),
                ..Default::default()
            }
        }),
        ..Default::default()
    };

    assert_eq!(do_thing(&factory, &mut differ, &mut patcher, &node), node);

    let node = Node {
        scene: "scene".to_owned(),
        ..Default::default()
    };

    assert_eq!(do_thing(&factory, &mut differ, &mut patcher, &node), node);
}

#[test]
fn diff_dispatch_vars() {
    let mut differ = NodeDiffer::new();
    differ.ident = vec![ClientId::Id(Uuid::new_v4())];

    let dispatch = DummyDispatch::new();
    let var = Arc::new(DummyVar(Uuid::new_v4()));

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap(),
                    var => var.as_variable().unwrap()
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Replace {
            data: NodeData {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => ScenePropData::Dispatch {
                        id: dispatch.get_id().unwrap(),
                        rx: true,
                        tx: true
                    },
                    var => ScenePropData::Variable {
                        id: var.get_id(),
                        rx: true,
                        tx: true
                    },
                }),
                meta: str_hash_map!({})
            },
            children: str_hash_map!({})
        }
    );

    assert_eq!(
        sub,
        vec![
            SubDiff::ConnectDispatch {
                d: dispatch.clone(),
                rx: true,
                tx: true
            },
            SubDiff::ConnectVariable {
                v: var.clone(),
                rx: true,
                tx: true
            },
        ]
    );

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    dispatch => PatchOperation::Delete,
                    var => PatchOperation::Delete,
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );

    assert_eq!(
        sub,
        vec![
            SubDiff::DisconnectDispatch(dispatch.clone()),
            SubDiff::DisconnectVariable(var.clone()),
        ]
    );
}

#[test]
fn diff_dispatch_nested() {
    let mut differ = NodeDiffer::new();
    differ.ident = vec![ClientId::Id(Uuid::new_v4())];

    let dispatch = DummyDispatch::new();

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap()
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Replace {
            data: NodeData {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => ScenePropData::Dispatch {
                        id: dispatch.get_id().unwrap(),
                        rx: true,
                        tx: true
                    },
                }),
                meta: str_hash_map!({})
            },
            children: str_hash_map!({})
        }
    );

    assert_eq!(
        sub,
        vec![SubDiff::ConnectDispatch {
            d: dispatch.clone(),
            rx: true,
            tx: true
        },]
    );

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap(),
                }),
                children: str_hash_map!({
                    first => Node {
                        scene: "second".to_owned(),
                        props: str_hash_map!({
                            dispatch2 => dispatch.as_dispatch().unwrap()
                        }),
                        ..Default::default()
                    }
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: None,
            children: str_hash_map!({
                first => NodeDiff::Replace {
                    data: NodeData {
                        scene: "second".to_owned(),
                        props: str_hash_map!({
                            dispatch2 => ScenePropData::Dispatch {
                                id: dispatch.get_id().unwrap(),
                                rx: true,
                                tx: true
                            },
                        }),
                        meta: str_hash_map!({ })
                    },
                    children: str_hash_map!({})
                }
            })
        }
    );

    assert_eq!(
        sub,
        vec![SubDiff::ConnectDispatch {
            d: dispatch.clone(),
            rx: true,
            tx: true
        },]
    );

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap(),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: None,
            children: str_hash_map!({
                first => NodeDiff::Delete
            })
        }
    );

    assert_eq!(sub, vec![SubDiff::DisconnectDispatch(dispatch.clone())]);

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    dispatch => PatchOperation::Delete,
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );

    assert_eq!(sub, vec![SubDiff::DisconnectDispatch(dispatch.clone())]);
}

#[test]
fn diff_dispatch_perms() {
    let mut differ = NodeDiffer::new();
    let a = ClientId::Id(Uuid::new_v4());
    let b = ClientId::Id(Uuid::new_v4());

    differ.ident = vec![a.clone()];

    let dispatch = DummyDispatch::new();

    dispatch
        .set_permissions(Some(Arc::new(DispatchPermissions {
            rx: PermissionSet::allow_only(a.clone()),
            tx: PermissionSet::allow_only(a.clone()),
        })))
        .unwrap();

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap(),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Replace {
            data: NodeData {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => ScenePropData::Dispatch {
                        id: dispatch.get_id().unwrap(),
                        rx: true,
                        tx: true
                    },
                }),
                meta: str_hash_map!({})
            },
            children: str_hash_map!({})
        }
    );

    assert_eq!(
        sub,
        vec![SubDiff::ConnectDispatch {
            d: dispatch.clone(),
            rx: true,
            tx: true
        },]
    );

    dispatch
        .set_permissions(Some(Arc::new(DispatchPermissions {
            rx: PermissionSet::allow_only(a.clone()),
            tx: PermissionSet::deny_all(),
        })))
        .unwrap();

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap(),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    dispatch => PatchOperation::Insert(ScenePropData::Dispatch {
                        id: dispatch.get_id().unwrap(),
                        rx: true,
                        tx: false
                    }),
                })),
                meta: None
            }),
            children: HashMap::new()
        }
    );

    assert_eq!(
        sub,
        vec![
            SubDiff::DisconnectDispatch(dispatch.clone()),
            SubDiff::ConnectDispatch {
                d: dispatch.clone(),
                rx: true,
                tx: false
            },
        ]
    );

    dispatch
        .set_permissions(Some(Arc::new(DispatchPermissions {
            rx: PermissionSet::deny_all(),
            tx: PermissionSet::deny_all(),
        })))
        .unwrap();

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap(),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    dispatch => PatchOperation::Delete,
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );

    assert_eq!(sub, vec![SubDiff::DisconnectDispatch(dispatch.clone()),]);

    dispatch
        .set_permissions(Some(Arc::new(DispatchPermissions {
            rx: PermissionSet::deny_all(),
            tx: PermissionSet::allow_only(a.clone()),
        })))
        .unwrap();

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap(),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    dispatch => PatchOperation::Insert(ScenePropData::Dispatch {
                        id: dispatch.get_id().unwrap(),
                        rx: false,
                        tx: true
                    }),
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );

    assert_eq!(
        sub,
        vec![SubDiff::ConnectDispatch {
            d: dispatch.clone(),
            rx: false,
            tx: true
        },]
    );

    dispatch
        .set_permissions(Some(Arc::new(DispatchPermissions {
            rx: PermissionSet::allow_only(b.clone()),
            tx: PermissionSet::allow_only(b.clone()),
        })))
        .unwrap();

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    dispatch => dispatch.as_dispatch().unwrap(),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    dispatch => PatchOperation::Delete,
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );

    assert_eq!(sub, vec![SubDiff::DisconnectDispatch(dispatch.clone()),]);
}

#[test]
fn value_perms() {
    let mut differ = NodeDiffer::new();
    let a = ClientId::Id(Uuid::new_v4());
    let b = ClientId::Id(Uuid::new_v4());

    differ.ident = vec![a.clone()];

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Replace {
            data: NodeData {
                scene: "scene".to_owned(),
                props: str_hash_map!({}),
                meta: str_hash_map!({})
            },
            children: str_hash_map!({})
        }
    );

    assert!(sub.is_empty());

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    value => 5.as_value_with_perms(PermissionSet::deny_all()),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(diff, NodeDiff::Same);
    assert!(sub.is_empty());

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    value => 5.as_value_with_perms(PermissionSet::allow_all()),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    value => PatchOperation::Insert(ScenePropData::Value(json!(5))),
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );
    assert!(sub.is_empty());

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    value => 5.as_value_with_perms(PermissionSet::deny_all()),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    value => PatchOperation::Delete,
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );
    assert!(sub.is_empty());

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    value => 5.as_value_with_perms(PermissionSet::allow_only(a.clone())),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    value => PatchOperation::Insert(ScenePropData::Value(json!(5))),
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );
    assert!(sub.is_empty());

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    value => 5.as_value_with_perms(PermissionSet::allow_only(b.clone())),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    value => PatchOperation::Delete,
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );
    assert!(sub.is_empty());

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    value => 5.as_value_with_perms(PermissionSet::allow_only(a.clone())),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    value => PatchOperation::Insert(ScenePropData::Value(json!(5))),
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );
    assert!(sub.is_empty());

    let (diff, sub) = differ
        .push_node_tree(
            Some(Node {
                scene: "scene".to_owned(),
                props: str_hash_map!({
                    value => 6.as_value_with_perms(PermissionSet::allow_only(a.clone())),
                }),
                ..Default::default()
            })
            .as_ref(),
        )
        .unwrap();

    assert_eq!(
        diff,
        NodeDiff::Merge {
            data: Some(NodeDataPatch {
                scene: None,
                props: Some(str_hash_map!({
                    value => PatchOperation::Insert(ScenePropData::Value(json!(6))),
                })),
                meta: None
            }),
            children: str_hash_map!({})
        }
    );
    assert!(sub.is_empty());
}
