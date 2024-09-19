
use std::hash::{Hash, Hasher};

use chimera_vnode_renderer::node::RenderContext;
use serde_json::json;

use crate::{prelude::*, entity::entity_instance::EntityInstanceData};

#[derive(Debug)]
struct TestEntity where Self: Sync + Send {
    pub id: Id,
    pub t: &'static str,
    pub render_fn: Option<Box<dyn Fn(EntityRenderContext)->Option<RenderFn> + Send + Sync>>,
    pub save_data: Option<Value>,
    pub owner: Option<Id>,
    pub handle_calls: Vec<Message>,
    pub query_calls: Mutex<Vec<Query>>,
    #[allow(clippy::type_complexity)]
    pub handle_fn: Option<Box<dyn Fn(&Message)->HandleResult + Send + Sync>>,
    #[allow(clippy::type_complexity)]
    pub query_fn: Option<Box<dyn Fn(&Query)->QueryResult + Send + Sync>>,
}

impl Entity for TestEntity {
    fn get_id(&self) -> Id {
        self.id.clone()
    }

    fn get_type(&self) -> &'static str {
        self.t
    }

    fn save(&self) -> BoxedResult<Option<Value>> {
        Ok(self.save_data.clone())
    }

    fn set_parent(&mut self, owner: Option<Id>) {
        self.owner = owner;
    }

    fn handle(&mut self, msg: &mut Message) -> HandleResult {
        self.handle_calls.push(msg.clone());
        match &self.handle_fn {
            Some(f) => f(msg),
            None => Ok(None),
        }
    }

    fn query(&self, query: &mut Query) -> QueryResult {
        self.query_calls.lock().unwrap().push(query.clone());
        match &self.query_fn {
            Some(f) => f(query),
            None => Ok(Effect::Value(Value::Null)),
        }
    }

    fn render(&self, context: EntityRenderContext) -> BoxedResult<Option<RenderFn>> {
        Ok(self.render_fn.as_ref().and_then(|f| f(context)))
    }
}

impl TestEntity {
    pub fn new(f: impl FnOnce(&mut Self)) -> (Id, Arc<RwLock<Self>>) {
        let id = Id::Id(Uuid::new_v4());

        (id.clone(), Arc::new(RwLock::new({
            let mut s = Self {
                id,
                t: "test-entity",
                render_fn: None,
                save_data: None,
                owner: None,
                handle_calls: Vec::new(),
                query_calls: Mutex::new(Vec::new()),
                handle_fn: None,
                query_fn: None,
            };

            f(&mut s);

            s
        })))
    }
}

fn render_list(list: &Arc<EntityList>) -> chimera_vnode_renderer::node::Node {
    scene!("list", children => list.render().unwrap())(RenderContext::new("/root"))
}

#[test]
fn el_basic() {
    let list = EntityList::new();

    let (_, a) = TestEntity::new(|e| {
        e.render_fn = Some(Box::new(|_| Some(scene!("asd"))));
    });

    list.push_entity(None, EntityRef::Owned(a.clone()), true).unwrap();

    render_list(&list);
}

#[test]
fn el_send() {
    let list = EntityList::new();

    let (ida, a) = TestEntity::new(|_| ());
    
    list.push_entity(None, EntityRef::Owned(a.clone()), true).unwrap();

    list.send(Message::to(ida.clone()).with_json_value(json!({
        "hello": "world"
    }))).unwrap();

    assert_eq!(a.read().unwrap().handle_calls, vec![
        Message::to(ida).with_json_value(json!({
            "hello": "world"
        }))
    ]);
}

#[test]
fn el_pause() {
    let list = Arc::new(EntityList::new());

    let (ida, a) = TestEntity::new(|_| ());
    
    list.push_entity(None, EntityRef::Owned(a.clone()), true).unwrap();

    list.send(Message::to(ida.clone()).with_json_value(json!({
        "hello": "world"
    }))).unwrap();

    assert_eq!(a.read().unwrap().handle_calls, vec![
        Message::to(ida.clone()).with_json_value(json!({
            "hello": "world"
        }))
    ]);

    let pause_guard = list.pause(&ida).unwrap();

    list.send(Message::to(ida.clone()).with_json_value(json!({
        "hello": "world2"
    }))).unwrap();

    assert_eq!(a.read().unwrap().handle_calls, vec![
        Message::to(ida.clone()).with_json_value(json!({
            "hello": "world"
        }))
    ]);

    drop(pause_guard);

    assert_eq!(a.read().unwrap().handle_calls, vec![
        Message::to(ida.clone()).with_json_value(json!({
            "hello": "world"
        })),
        Message::to(ida).with_json_value(json!({
            "hello": "world2"
        }))
    ]);
}

#[test]
fn el_send_immediate() {
    let list = Arc::new(EntityList::new());

    let (ida, a) = TestEntity::new(|_| ());
    
    list.push_entity(None, EntityRef::Owned(a.clone()), true).unwrap();

    list.send(Message::to(ida.clone()).immediate().with_json_value(json!({
        "hello": "world"
    }))).unwrap();

    assert_eq!(a.read().unwrap().handle_calls, vec![
        Message::to(ida.clone()).immediate().with_json_value(json!({
            "hello": "world"
        }))
    ]);

    let pause_guard = list.pause(&ida).unwrap();

    let status = list.send(Message::to(ida.clone()).immediate().with_json_value(json!({
        "hello": "world2"
    }))).unwrap();

    assert_eq!(status, SendStatus::Paused);

    assert_eq!(a.read().unwrap().handle_calls, vec![
        Message::to(ida.clone()).immediate().with_json_value(json!({
            "hello": "world"
        }))
    ]);

    drop(pause_guard);

    assert_eq!(a.read().unwrap().handle_calls, vec![
        Message::to(ida).immediate().with_json_value(json!({
            "hello": "world"
        }))
    ]);
}

#[test]
fn el_children_basic() {
    let list = EntityList::new();

    let (ida, a) = TestEntity::new(|e| {
        e.t = "child-entity";
        e.save_data = Some(json!({}));
    });
    let (idc, c) = TestEntity::new(|e| {
        e.save_data = Some(json!({}));
    });

    list.push_entity(None, EntityRef::Owned(c.clone()), true).unwrap();
    list.push_entity(Some(idc.clone()), EntityRef::Owned(a.clone()), true).unwrap();

    assert_eq!(list.dump().unwrap(), json!({
        "entities": [
            {
                "entity_type": "child-entity",
                "data": {}
            },
            {
                "entity_type": "test-entity",
                "data": {}
            }
        ]
    }));
}

#[test]
fn el_children_sync() {
    let list = EntityList::new();

    let (ida, a) = TestEntity::new(|e| {
        e.save_data = Some(json!({}));
    });
    let (idc, c) = TestEntity::new(|e| {
        e.t = "child-entity";
        e.save_data = Some(json!({}));
    });

    list.push_entity(None, EntityRef::Owned(a.clone()), true).unwrap();

    assert_eq!(list.dump().unwrap(), json!({
        "entities": [
            {
                "entity_type": "test-entity",
                "data": {}
            }
        ]
    }));

    list.push_entity(Some(ida), EntityRef::Owned(c.clone()), true).unwrap();

    assert_eq!(list.dump().unwrap(), json!({
        "entities": [
            {
                "entity_type": "child-entity",
                "data": {}
            },
            {
                "entity_type": "test-entity",
                "data": {}
            }
        ]
    }));

    let d = list.pop_entity(&idc, false).unwrap().unwrap();
    assert_eq!(d.entity_type, "child-entity");
    assert_eq!(d.data, json!({}));

    assert_eq!(list.dump().unwrap(), json!({
        "entities": [
            {
                "entity_type": "test-entity",
                "data": {}
            }
        ]
    }));
}

#[derive(Debug)]
pub struct Item(Id);

impl StaticallyTypedEntity for Item {
    const TYPE: &'static str = "item";
}

impl EntityFactory for Item {
    type Param = ();

    fn new(_: &EntityList) -> Arc<RwLock<Self>> {
        Arc::new(RwLock::new(Self(Id::Id(Uuid::new_v4()))))
    }

    fn load(_: &EntityList, data: Value) -> BoxedResult<Arc<RwLock<Self>>> {
        Ok(Arc::new(RwLock::new(Self(serde_json::from_value(data)?))))
    }
}

impl Entity for Item {
    fn get_id(&self) -> Id {
        self.0.clone()
    }

    fn get_type(&self) -> &'static str {
        Self::TYPE
    }

    fn save(&self) -> BoxedResult<Option<Value>> {
        Ok(Some(serde_json::to_value(self.0.clone())?))
    }

    fn render(&self, _ctx: EntityRenderContext) -> BoxedResult<Option<RenderFn>> {
        Ok(Some(scene!("item")))
    }
}

#[derive(Debug)]
pub struct Inventory {
    id: Id,
    slots: HashMap<i32, Id>,
    overflow: Vec<Id>,
    escrow: HashSet<Id>,
}

impl StaticallyTypedEntity for Inventory {
    const TYPE: &'static str = "inventory";
}

impl EntityFactory for Inventory {
    fn new(e: &EntityList) -> Arc<RwLock<Self>> {
        Arc::new(RwLock::new(Self{
            id: Id::Id(Uuid::new_v4()),
            slots: HashMap::new(),
            overflow: Vec::new(),
            escrow: HashSet::new()
        }))
    }
}

#[entity]
impl Entity for Inventory {
    fn get_id(&self) -> Id {
        self.id.clone()
    }

    fn get_type(&self) -> &'static str {
        Self::TYPE
    }

    fn save(&self) -> BoxedResult<Option<Value>> {
        Ok(Some(json!({
            "id": self.id,
            "items": self.slots,
            "overflow": self.overflow,
            "escrow": self.escrow
        })))
    }

    /**
     * 3rd party: Please move the contents of slot 0 in inventory X to slot 1 in inventory Y
     * Inventory X: (move)
     *   - I have an item in that slot
     *   - I remove the item from the slot
     *   - I add an escrow entry for the item, in case I don't get a confirm back
     *   - I tell Inventory Y to insert the item into slot 1, along with my id and slot 0 as the FF data
     * Inventory Y: (insert)
     *   - I have been given an item to put in slot 1
     *   - I have an item in that slot
     *   - I keep a reference to the item I have
     *   - I compare exchange ownership of it w.r.t. Inventory X - fallible
     *   - I put the new item in the slot
     *   - I tell Inventory X that I received the item
     *   - I add an escrow entry for my item, in case I don't get a confirm back
     *   - I tell Inventory X to insert my item into slot 0, with no FF data (to prevent an infinite loop of inserts)
     * Inventory X: (confirm)
     *   - I have received confirmation that Inventory Y has my given item, so I remove it from escrow
     * Inventory X: (insert)
     *   - I have been given an item to put in slot 0
     *   - In the interim, another party has given me an item to put in that slot
     *   - I have no FF data, so I have to insert the new item into the overflow buffer to prevent item loss
     *   - I put the new item in the overflow buffer
     *   - I tell Inventory Y that I have received the item
     * Inventory Y: (confirm)
     *   - Inventory X has received my item, so I remove it from escrow
     */

    #[action]
    fn r#move(&mut self, from: i32, to: i32, next: Id) -> HandleResult {
        let mut e = Vec::new();

        if let Some(item) = self.slots.remove(&from) {
            self.escrow.insert(item.clone());
            
            e.push({
                effects::send({
                    Message::to(next.clone())
                        .from(self.get_id())
                        .with_message(InventoryMessage::Insert {
                            item,
                            slot: to,
                            feed_forward: Some((self.get_id(), from)),
                        })?
                })
            });
        }

        if e.is_empty() {
            Ok(None)
        } else {
            Ok(Some(effects::sequential(e)))
        }
    }

    #[action]
    fn insert(&mut self, #[from] sender: Option<Id>, item: Id, slot: i32, feed_forward: Option<(Id, i32)>) -> HandleResult {
        let sender = sender
            .ok_or_else(|| "Message::from must be specified".to_boxed_error())
            .map_err_with_backtrace()?;

        with_sequential(|e| {
            e.push(effects::compare_exchange_entity_parent(item.clone(), Some(sender.clone()), Some(self.get_id())));

            e.push(effects::send(InventoryMessage::insert_for_real_this_time(self.get_id(), item, slot, feed_forward)));

            e.push(effects::send({
                Message::to(sender)
            }));

            Ok(()) 
        })
    }

    #[action]
    fn insert_for_real_this_time(&mut self, item: Id, slot: i32, feed_forward: Option<(Id, i32)>) -> HandleResult {
        with_sequential(|e| {
            let prev = self.slots.insert(slot, item);
    
            if let Some(prev) = prev {
                if let Some((ff_id, ff_slot)) = feed_forward {
                    self.escrow.insert(prev.clone());
        
                    e.push(effects::send({
                        Message::to(ff_id)
                            .from(self.get_id())
                            .with_message(InventoryMessage::Insert {
                                item: prev,
                                slot: ff_slot,
                                feed_forward: None
                            })?
                    }));
                } else {
                    self.overflow.push(prev);
                }
            }

            Ok(()) 
        })
    }

    #[action]
    fn confirm_receipt(&mut self, id: Id) {
        self.escrow.remove(&id);
    }

    fn render(&self, _context: EntityRenderContext) -> BoxedResult<Option<RenderFn>> {
        Ok(Some(scene!(
            "inventory",
            children => self.slots.iter()
                .filter_map(|(i, item)| item.render(EntityRenderContext::Custom(Value::Null)).inside_out_with(|x| (i.to_string(), x)))
                .try_collect::<HashMap<_, _>>()?
        )))
    }
}

#[test]
// #[tracing_test::traced_test]
fn inventory() {

    let list = EntityList::new();

    let ia = list.add_new_entity::<Inventory>(None).unwrap();
    let ib = list.add_new_entity::<Inventory>(None).unwrap();

    let item = ChildEntity::new::<Item>(&list, ).unwrap();
    let id_item = item.get_id();

    list.assert_entities(hash_map!({
        [ia.to_string()] => json!({
            "data": {
                "id": {
                    "Id": ia.to_string()
                },
                "items": {},
                "overflow": []
            },
            "entity_type": "inventory",
        }),
        [ib.to_string()] => json!({
            "data": {
                "id": {
                    "Id": ib.to_string()
                },
                "items": {},
                "overflow": []
            },
            "entity_type": "inventory",
        })
    }));

    list.send({
        Message::to(ia.clone())
            .with_message(InventoryMessage::Insert {
                item: item.save().unwrap().unwrap(),
                to: 0
            }).unwrap()
    }).unwrap();

    list.assert_entities(hash_map!({
        [ia.to_string()] => json!({
            "data": {
                "id": {
                    "Id": ia.to_string()
                },
                "items": {
                    "0": {
                        "data": {
                            "Id": id_item.to_string()
                        },
                        "entity_type": "item"
                    }
                },
                "overflow": []
            },
            "entity_type": "inventory",
        }),
        [ib.to_string()] => json!({
            "data": {
                "id": {
                    "Id": ib.to_string()
                },
                "items": {},
                "overflow": []
            },
            "entity_type": "inventory",
        }),
        [id_item.to_string()] => json!({
            "data": {
                "Id": id_item.to_string()
            },
            "entity_type": "item"
        })
    }));

    list.send({
        Message::to(ia.clone())
            .next(ib.clone())
            .with_message(InventoryMessage::Move {
                from: 0,
                to: 3,
            }).unwrap()
    }).unwrap();

    list.assert_entities(hash_map!({
        [ia.to_string()] => json!({
            "data": {
                "id": {
                    "Id": ia.to_string()
                },
                "items": {},
                "overflow": []
            },
            "entity_type": "inventory"
        }),
        [ib.to_string()] => json!({
            "data": {
                "id": {
                    "Id": ib.to_string()
                },
                "items": {
                    "3": {
                        "data": {
                            "Id": id_item.to_string()
                        },
                        "entity_type": "item"
                    }
                },
                "overflow": []
            },
            "entity_type": "inventory"
        }),
        [id_item.to_string()] => json!({
            "data": {
                "Id": id_item.to_string()
            },
            "entity_type": "item"
        })
    }));
}

extern crate test;
use test::Bencher;

#[bench]
pub fn bench_inventory_move(b: &mut Bencher) {

    let list = EntityList::new();

    let ia = list.add_new_entity::<Inventory<_>>().unwrap();

    let item = ChildEntity::new::<Item>(&list).unwrap();
    list.send({
        Message::to(ia.clone())
            .with_message(InventoryMessage::Insert {
                item: item.save().unwrap().unwrap(),
                to: 0
            }).unwrap()
    }).unwrap();

    b.iter(|| {
        list.send({
            Message::to(ia.clone())
                .next(ia.clone())
                .with_message(InventoryMessage::Move {
                    from: 0,
                    to: 1,
                }).unwrap()
        }).unwrap();

        list.send({
            Message::to(ia.clone())
                .next(ia.clone())
                .with_message(InventoryMessage::Move {
                    from: 1,
                    to: 0,
                }).unwrap()
        }).unwrap();
    });
}

#[bench]
pub fn message_serialize(b: &mut Bencher) {
    let id = Id::Id(Uuid::new_v4());

    b.iter(|| {
        test::black_box({
            serde_json::to_value({
                Message::to(id.clone())
                    .next(id.clone())
                    .with_message(InventoryMessage::Move {
                        from: 0,
                        to: 1,
                    }).unwrap()
            })
        })
    });
}
