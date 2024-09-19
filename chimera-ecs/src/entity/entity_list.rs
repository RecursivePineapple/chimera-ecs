
use std::any::Any;

use crate::prelude::*;
use crate::entity::entity_instance::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize, PartialOrd, Ord)]
pub enum Id {
    Id(Uuid),
    Singleton(String)
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Id::Id(id) => f.write_str(&id.to_string()),
            Id::Singleton(s) => f.write_str(s),
        }
    }
}

impl Id {
    pub fn new_uuid() -> Self {
        Id::Id(Uuid::new_v4())
    }
}

pub type EntityCreator<Context> = dyn Fn(&EntityList<Context>, &str, Option<Value>)->Option<EntityReference<Context>> + Sync + Send + 'static;

impl<C: Context> Effect<C> {
    fn consume(self, effects: &mut Vec<Effect<C>>) -> BoxedResult {
        match self {
            Effect::None(_, _) | Effect::Value(_, _) => Ok(()),
            Effect::Err(e, _) => Err(e),
            d @ Effect::Deferred(_, _) => {
                effects.push(d);
                Ok(())
            },
        }
    }
}

#[derive(Serialize, Deserialize)]
struct EntityListData {
    entities: Vec<EntityInstanceData>,
}

type EntityHashMap<C> = HashMap<Id, Arc<RwLock<EntityInstance<C>>>>;

pub struct EntityList<C: Context = ()> {
    pub context: C,
    this: Weak<EntityList<C>>,
    entities: RwLock<EntityHashMap<C>>,
    paused: RwLock<HashMap<Id, Vec<Message>>>,
    tick: Mutex<HashSet<Id>>,
    render_lock: RwLock<()>,
    on_request_rerender: BroadcastEvent<()>,
}

impl std::fmt::Debug for EntityList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EntityList").finish()
    }
}

impl<C: Context> EntityList<C> {
    pub fn new() -> Arc<Self> where C: Default {
        Arc::new_cyclic(|w| Self {
            context: C::default(),
            this: w.clone(),
            entities: RwLock::new(HashMap::new()),
            paused: RwLock::new(HashMap::new()),
            tick: Mutex::new(HashSet::new()),
            render_lock: RwLock::new(()),
            on_request_rerender: BroadcastEvent::new(),
        })
    }

    fn add_impl(
        &self,
        entities: &mut EntityHashMap<C>,
        effects: &mut Vec<Effect<C>>,
        inst: EntityInstance<C>,
        is_add: bool
    ) -> BoxedResult<Id> {
        let id = inst.id.clone();

        let _span = tracing::debug_span!("push entity", parent = ?inst.parent, id = ?inst.id, entity_type = ?inst.entity_type, is_add).entered();

        tracing::info!(what = "pushing entity", parent = ?inst.parent, id = ?inst.id, entity_type = ?inst.entity_type, is_add);

        if entities.contains_key(&id) {
            return Err(format!("cannot add entity {} because an entity with the same id is already inserted", inst.id).to_boxed_error());
        }

        let inst = Arc::new(RwLock::new(inst));

        entities.insert(id.clone(), inst.clone());

        inst.with_immut(
            |inst| -> BoxedResult<()> {
                inst.try_deref()?.with_mut(
                    |e| -> BoxedResult<()> {
                        if is_add {
                            e.on_added().consume(effects).map_err_explain("Entity::on_added() returned error")?;
                        }
                
                        e.on_pushed().consume(effects).map_err_explain("Entity::on_pushed() returned error")?;
                
                        Ok(())
                    }
                )??;

                Ok(())
            }
        )??;

        Ok(id)
    }

    #[allow(clippy::type_complexity)]
    fn remove_impl(
        &self,
        entities: &mut EntityHashMap<C>,
        effects: &mut Vec<Effect<C>>,
        id: &Id,
        is_remove: bool
    ) -> BoxedResult<Option<Arc<RwLock<EntityInstance<C>>>>> {
        let _span = tracing::debug_span!("pop entity", ?id, is_remove).entered();        

        if let Some(inst) = entities.remove(id) {
            inst.with_immut(|inst| -> BoxedResult<()> {
                for c in &inst.children {
                    self.remove_impl(entities, effects, c, is_remove)?;
                }
    
                if let Ok(e) = inst.try_deref() {
                    e.with_mut(|e| -> BoxedResult<_> {
                        e.on_popped().consume(effects).map_err_explain("Entity::on_popped() returned error")?;

                        if is_remove {
                            e.on_removed().consume(effects).map_err_explain("Entity::on_removed() returned error")?;
                        }

                        Ok(())
                    }).flatten()?;
                }

                tracing::info!(what = "popped entity", ?id, entity_type = ?inst.entity_type, is_remove);

                Ok(())
            }).flatten()?;

            Ok(Some(inst))
        } else {
            Ok(None)
        }
    }

    fn send_impl(
        &self,
        inst: &EntityInstance<C>,
        effects: &mut Vec<Effect<C>>,
        e: &mut dyn Entity<C>,
        msgs: impl Iterator<Item = Message>
    ) -> BoxedResult<()> {
        
        for mut msg in msgs {
            while msg.payload.len() > 0 {
                tracing::info!(what = "sending message", dest = ?inst.id, payload = ?msg);
                
                e.handle(&mut msg).consume(effects).map_err_explain("Entity::handle() returned error")?;
            }
        }

        inst.mark_dirty();

        if let Some(o) = &inst.parent {
            effects.push(effects::mark_dirty(o.clone()));
        }

        self.on_request_rerender.invoke(&())?;

        Ok(())
    }

    fn send_impl2(&self, effects: &mut Vec<Effect<C>>, dest: &Id, msgs: impl Iterator<Item = Message>) -> BoxedResult<()> {

        let inst = self.entities.with_immut(
            |e| e.get(&dest).cloned().ok_or_else(|| format!("could not find entity {dest}"))
        )??;

        inst.with_immut(|inst| -> BoxedResult<_> {
            inst.try_deref()?.with_mut(|e| {
                self.send_impl(&inst, effects, e, msgs)
            }).flatten()
        }).flatten()?;

        Ok(())
    }

    pub fn with_effects<R>(&self, f: impl FnOnce(&mut Vec<Effect<C>>)->R) -> BoxedResult<R> {
        let mut effects = Vec::new();

        let r = (f)(&mut effects);

        for e in effects {
            e.evaluate(self)?;
        }

        Ok(r)
    }

    pub fn send(&self, msg: Message) -> BoxedResult<SendStatus> {
        let dest = msg.dest.clone();

        let _span = tracing::debug_span!("message send", ?dest, message_type = %msg.message_type().unwrap_or(CowStr::Borrowed("unknown"))).entered();

        let is_paused = self.paused.with_immut(
            |p| p.contains_key(&dest)
        )?;

        match &msg.send_mode {
            SendMode::Immediate => {
                if is_paused {
                    return Ok(SendStatus::Paused);
                }
            },
            SendMode::QueueIfPaused => {
                if is_paused {
                    self.paused.with_mut(
                        |p| {
                            if let Some(v) = p.get_mut(&dest) {
                                v.push(msg);
                            }
                        }
                    )?;

                    return Ok(SendStatus::Paused);
                }
            },
        }

        self.with_effects(|effects| {
            self.send_impl2(effects, &dest, vec![msg].into_iter())
        }).flatten()?;

        self.on_request_rerender.invoke(&())?;

        Ok(SendStatus::Sent)
    }

    pub fn query(&self, mut query: Query) -> BoxedResult<QueryResponse> {
        let dest = query.dest.clone();

        let is_paused = self.paused.with_immut(
            |p| p.contains_key(&dest)
        )?;

        if is_paused {
            return Ok(QueryResponse::Paused);
        }

        let inst = self.entities.with_immut(
            |e| e.get(&dest).cloned().ok_or_else(|| format!("could not find entity {dest}").to_boxed_error())
        ).flatten()?;

        let q = inst.with_immut(|inst| -> BoxedResult<_> {
            inst.try_deref()?.with_immut(|e| {
                e.query(&mut query)
            })
        }).flatten()?;

        let q = q.evaluate(self)
            .map_err_explain_with(|| format!("could not evaluate query result from entity {dest}"))?;

        Ok(QueryResponse::Response(q))
    }

    pub fn open_dispatch(&self, id: Id) -> Arc<EntityDispatch> {
        EntityDispatch::new(self.this.clone(), id)
    }

    /// When the entity list is accessed recursively, it will deadlock.
    /// This Arc MUST be used in an external thread, such as an async task.
    pub unsafe fn get_ref(&self) -> Arc<Self> {
        self.this.upgrade().expect("expected self to upgrade")
    }

    /// Gets a direct reference to an entity, or `None` if it does not exist.
    /// Be VERY careful with this, as there are several things an EntityList does to ensure consistency.
    pub unsafe fn get_entity(&self, id: &Id) -> BoxedResult<Option<EntityReference<C>>> {
        self.entities.with_immut(|l| {
            l.get(id)
                .and_then(|inst| {
                    inst.with_immut(|e| {
                        e.try_get_entity().ok()
                    }).inside_out()
                })
                .inside_out()
        }).flatten()
    }

    /// Gets a direct reference to an entity, or None if it does not exist.
    /// Be VERY careful with this, as there are several things an EntityList does to ensure consistency.
    pub unsafe fn with_typed_entity<E: Entity + 'static, F: FnOnce(&E)->R, R>(&self, id: &Id, f: F) -> BoxedResult<Option<R>> {
        self.entities.with_immut(|l| {
            let inst = match l.get(id) {
                Some(inst) => inst,
                None => {
                    return Ok(None);
                }
            };

            inst.with_immut(|e| {
                let e = match e.try_get_entity().ok() {
                    Some(e) => e,
                    None => {
                        return Ok(None);
                    }
                };

                e.with_immut(|x| {
                    if let Some(e) = (x as &dyn Any).downcast_ref::<E>() {
                        Ok(Some(f(e)))
                    } else {
                        Err(format!(
                            "entity {id} ({}) could not be converted to the request type {}",
                            x.get_type(),
                            std::any::type_name::<E>()
                        ).to_boxed_error())
                    }
                }).flatten()
            }).flatten()
        }).flatten()
    }

    pub fn pause(&self, id: &Id) -> BoxedResult<()> {
        self.paused.with_mut(|l| {

            let e = l.entry(id.clone());

            e.or_insert_with(Vec::new);
        })
    }

    pub fn resume(&self, id: &Id) -> BoxedResult<()> {
        let v = self.paused.with_mut(|l| {
            l.remove(id).and_then(|v| (!v.is_empty()).then_some(v))
        })?;

        if let Some(v) = v {
            self.with_effects(|effects| {
                self.send_impl2(effects, id, v.into_iter())
            }).flatten()?;

            self.on_request_rerender.invoke(&())?;
        }

        Ok(())
    }

    pub fn mark_dirty(&self, id: &Id) -> BoxedResult<()> {
        let inst = self.entities.with_immut(
            |e| e.get(&id).cloned().ok_or_else(|| format!("could not find entity {id}").to_boxed_error())
        ).flatten()?;

        let owner = inst.with_immut(|inst| {
            inst.mark_dirty();
            inst.parent.clone()
        })?;

        if let Some(owner) = owner {
            self.mark_dirty(&owner)?;
        }

        self.on_request_rerender.invoke(&())?;
        
        Ok(())
    }

    pub fn set_should_tick(&self, id: &Id, should_tick: bool) -> BoxedResult<()> {
        self.tick.with_mut(|tick| {
            if should_tick {
                tick.insert(id.clone());
            } else {
                tick.remove(id);
            }
        })?;

        Ok(())
    }

    pub fn push_entity(&self, parent: Option<Id>, e: EntityRef<C>, is_add: bool) -> BoxedResult<Id> {
        let inst = EntityInstance::new(parent, e)?;
        let id = inst.id.clone();

        self.with_effects(|effects| {
            self.entities.with_mut(
                |l| self.add_impl(l, effects, inst, is_add)
            ).flatten()
        }).flatten()?;

        self.on_request_rerender.invoke(&())?;

        Ok(id)
    }

    pub fn pop_entity(&self, id: &Id, is_remove: bool) -> BoxedResult<Option<EntityInstanceData>> {
        let e = self.with_effects(|effects| {
            self.entities.with_mut(
                |entities| self.remove_impl(entities, effects, id, is_remove)
            ).flatten()
        }).flatten()?;

        match e {
            Some(inst_l) => {
                self.on_request_rerender.invoke(&())?;

                Ok(EntityInstanceData::save(&read_lock_with_timeout(&inst_l)?.try_deref()?)?)
            },
            None => {
                Ok(None)
            }
        }
    }

    pub fn atomic_pop_push(
        &self,
        pop: Id,
        is_remove: bool,
        parent: Option<Id>,
        push: EntityRef<C>,
        is_add: bool
    ) -> BoxedResult<(Option<EntityInstanceData>, Id)> {
        let inst = EntityInstance::new(parent, push)?;
        let id = inst.id.clone();

        let data = self.with_effects(|effects| {
            self.entities.with_mut(|l| {
                let popped = self.remove_impl(l, effects, &pop, is_remove)
                    .map_err_explain("could not pop entity in atomic_pop_push")?;

                let popped = match popped {
                    Some(p) => {
                        p.with_immut(|p| BoxedResult::Ok(EntityInstanceData::save(&p.try_deref()?)))???
                    },
                    None => None
                };

                self.add_impl(l, effects, inst, is_add)
                    .map_err_explain("could not push entity in atomic_pop_push")?;

                Ok(popped)
            }).flatten()
        }).flatten()?;

        self.on_request_rerender.invoke(&())?;

        Ok((data, id))
    }

    pub fn add_new_entity<E: Entity<C> + PureEntity<C>>(&self, parent: Option<Id>) -> BoxedResult<Id> {
        self.push_entity(parent, EntityRef::Owned(E::new().evaluate(self)?), true)
    }

    pub fn load_entity<E: Entity<C> + PureEntity<C>>(&self, parent: Option<Id>, state: Value) -> BoxedResult<Id> {
        let e = E::new().evaluate(self)?;

        e.with_mut(|e| {
            e.load(state).evaluate(self)
        }).flatten().map_err_explain("could not load entity")?;

        self.push_entity(parent, EntityRef::Owned(e), true)
    }

    pub fn load_entity_generic(&self, factory: &EntityCreator<C>, parent: Option<Id>, data: EntityInstanceData) -> BoxedResult<Id> {
        self.push_entity(parent, EntityRef::Owned(data.load_generic(self, factory)
            .map_err_explain("could not load entity")?), true)
    }

    pub fn ensure_singleton<E: Entity<C> + PureEntity<C> + StaticallyTypedEntity>(&self) -> BoxedResult<Id> {
        let id = E::get_singleton_id();

        if !self.entities.with_immut(|e| e.contains_key(&id))? {
            self.add_new_entity::<E>(None)
        } else {
            Ok(id)
        }
    }

    pub fn tick(&self) -> BoxedResult<()> {
        self.with_effects(|effects| {
            self.tick.with_mut(|to_tick| {
                if to_tick.is_empty() {
                    return Ok(());
                }

                self.entities.with_immut(|entities| -> BoxedResult<_> {
                    let mut invalid = Vec::new();

                    for id in to_tick.iter() {
                        match entities.get(&id) {
                            Some(inst) => {
                                inst.with_immut(|inst| -> BoxedResult<_> {
                                    inst.try_deref()?.with_mut(|e| -> BoxedResult<_> {
                                        e.tick().consume(effects).map_err_explain("Entity::tick() returned error")?;
                                        Ok(())
                                    }).flatten()
                                }).flatten()?;
                            },
                            None => {
                                invalid.push(id.clone());
                            }
                        }
                    }

                    for id in invalid {
                        to_tick.remove(&id);
                    }

                    Ok(())
                }).flatten()
            }).flatten()
        }).flatten()?;

        self.on_request_rerender.invoke(&())?;

        Ok(())
    }

    pub fn with_paused_render<R>(&self, f: impl FnOnce()->R) -> BoxedResult<R> {
        self.render_lock.with_immut(|_| {
            (f)()
        })
    }

    pub fn render(&self) -> BoxedResult<HashMap<Id, Node>> {
        let _l = write_lock_with_timeout(&self.render_lock)
            .map_err_explain("could not lock render lock")?;

        let m: _ = self.entities.with_immut(|entities: &HashMap<Id, Arc<RwLock<EntityInstance<C>>>>| -> BoxedResult<_> {
            entities.iter()
                .filter_map(|(id, inst)| -> _ {
                    inst.with_mut(|inst| -> BoxedResult<_> {
                        let f: _ = inst.render()
                            .map_err_explain_with(|| format!("could not render entity {id}"))?;
                            
                        Ok(f.map(|f| (id.clone(), f)))
                    }).flatten().inside_out()
                })
                .try_collect()
        }).flatten()?;

        Ok(m)
    }

    pub fn on_request_rerender(&self) -> SubscriptionWatchRef<()> {
        &self.on_request_rerender
    }

    pub fn with_entities<R>(&self, f: impl FnOnce(&EntityHashMap<C>)->R) -> BoxedResult<R> {
        self.entities.with_immut(|l| {
            (f)(l)
        })
    }

    #[cfg(test)]
    pub fn dump(&self) -> BoxedResult<Value> {
        let mut data = EntityListData {
            entities: Vec::new()
        };

        for inst in read_lock_with_timeout(&self.entities)?.values() {
            let inst_l = read_lock_with_timeout(inst)?;

            if let Some(inst_data) = EntityInstanceData::save(&inst_l.try_deref()?)? {
                data.entities.push(inst_data);
            }
        }

        data.entities.sort_by_key(|e| e.entity_type.clone());

        Ok(serde_json::to_value(data)?)
    }

    #[cfg(debug_assertions)]
    pub fn dump_entities(&self) -> BoxedResult<Vec<(Id, CowStr, HashSet<Id>)>> {
        let mut data = Vec::new();

        for inst in read_lock_with_timeout(&self.entities)?.values() {
            let inst_l = read_lock_with_timeout(inst)?;

            data.push((inst_l.id.clone(), inst_l.entity_type.clone(), inst_l.children.clone()));
        }

        data.sort_by_key(|(id, ..)| id.clone());

        Ok(data)
    }

    #[cfg(test)]
    pub fn assert_entities(&self, mut expected: HashMap<String, Value>) {
        for inst in read_lock_with_timeout(&self.entities).unwrap().values() {
            let inst_l = read_lock_with_timeout(inst).unwrap();

            let data =  EntityInstanceData::save(&inst_l.try_deref().unwrap()).unwrap();

            if let Some(data) = data {
                match expected.remove(&inst_l.id.to_string()) {
                    Some(e) => {
                        assert_eq!(serde_json::to_value(data).unwrap(), e);
                    },
                    None => {
                        panic!("Found entity {} when it was not expected.\n{}", inst_l.id, serde_json::to_value(data).unwrap());
                    }
                }
            }
        }

        for (id, state) in expected {
            panic!("Expected entity {id} to exist, but it doesn't.\n{state}");
        }
    }

    // pub fn save(&self) -> BoxedResult<Value> {
    //     let mut data = EntityListData {
    //         entities: Vec::new()
    //     };

    //     for inst in read_lock_with_timeout(&self.entities)?.values() {
    //         let inst_l = read_lock_with_timeout(inst)?;

    //         if matches!(inst_l.entity, EntityRef::Owned(_)) && let Some(inst_data) = EntityInstanceData::save(&inst_l.try_deref()?)? {
    //             data.entities.push(inst_data);
    //         }
    //     }

    //     Ok(serde_json::to_value(data)?)
    // }

    // pub fn load(&self, state: Value, factory: &EntityCreator) -> BoxedResult<()> {
        
    //     let data: EntityListData = serde_json::from_value(state)?;

    //     let mut l = write_lock_with_timeout(&self.entities)?;

    //     l.clear();

    //     let mut effects = Vec::new();

    //     for e in data.entities {
    //         self.add_impl(&mut l, &mut effects, EntityInstance::new(EntityRef::Owned(e.load_generic(self, factory)?))?, false)?;
    //     }

    //     drop(l);

    //     for e in effects {
    //         e.evaluate(self)?;
    //     }

    //     Ok(())
    // }
}
