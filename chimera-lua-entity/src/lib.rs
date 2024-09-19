use std::{collections::HashMap, sync::{Arc, atomic::AtomicBool, Weak}, marker::PhantomData, borrow::Cow, pin::Pin};

use chimera_core::prelude::*;
use chimera_ecs::prelude::*;
use serde::{Serialize, Deserialize};
use serde_json::Value;
use streamunordered::{StreamUnordered, StreamYield};
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender, unbounded_channel};
use tokio_stream::{StreamExt, wrappers::UnboundedReceiverStream, Stream};
use chimera_utils::*;
use mlua::prelude::*;
use uuid::Uuid;

#[derive(Debug)]
pub(crate) struct RenderCache {
    pub nodes: HashMap<Id, Node>,
    pub dirty: HashMap<Id, AtomicBool>,
}

#[derive(Debug)]
pub struct LuaContext<C: Context> {
    pub(crate) tx: UnboundedSender<LuaContextAction<C>>,
}

#[derive(Debug)]
enum LuaEntityLifecycle {
    SetParent(Option<Id>),
    Added,
    Removed,
    Pushed,
    Popped,
    Ticked,
}

#[derive(Debug)]
enum LuaEntityAction<C: Context> {
    Rerender(std::sync::mpsc::Sender<Option<Node>>),
    Handle(Message, std::sync::mpsc::Sender<Effect<C>>),
    Query(Query, std::sync::mpsc::Sender<Effect<C, Response>>),
    Lifecycle(LuaEntityLifecycle, std::sync::mpsc::Sender<Effect<C>>),
}

#[derive(Debug)]
enum LuaContextAction<C: Context> {
    NewEntity {
        script: CowStr,
        entity: CowStr,
        rx: UnboundedReceiver<LuaEntityAction<C>>,
        param: Value,
    }
}

enum WorkerInput<C: Context> {
    Action(LuaContextAction<C>),
    Entity(LuaEntityAction<C>),
}

#[derive(Debug, Serialize, Deserialize)]
struct LuaNode {
    pub scene: String,
    pub props: HashMap<String, SceneProp>,
    pub meta: HashMap<String, Value>,
    pub children: HashMap<String, LuaNode>,
    pub visibility: Option<PermissionSet>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SceneProp {
    Value {
        value: Value,
        #[serde(default)]
        perms: Option<PermissionSet>
    },
    Dispatch {
        dispatch: Uuid,
        #[serde(default)]
        tx: Option<PermissionSet>,
        #[serde(default)]
        rx: Option<PermissionSet>
    },
    Variable {
        variable: Uuid,
        #[serde(default)]
        read: Option<PermissionSet>,
        #[serde(default)]
        write: Option<PermissionSet>
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum PermissionSet {
    #[serde(rename = "white_list")]
    WhiteList(Vec<ClientId>),
    #[serde(rename = "black_list")]
    BlackList(Vec<ClientId>)
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ClientId {
    Id(Uuid),
    Singleton(String),
}

fn spawn<C: Context>(e: Weak<EntityList<C>>, mut rx: UnboundedReceiver<LuaContextAction<C>>) {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .thread_name("tokio-runtime-lua-context")
        .build()
        .unwrap();

    let ls = tokio::task::LocalSet::new();

    let lua = Lua::new();

    let mut input = StreamUnordered::new();

    let action_idx = input.insert({
        rx.pipe(UnboundedReceiverStream::new)
            .map(WorkerInput::Action)
            .pipe(|s| Box::new(s)as Box<dyn Stream<Item = WorkerInput<C>> + Unpin>)
    });

    struct Entity {
        pub spec: LuaOwnedTable,
        pub state: LuaOwnedTable,
    }

    let mut entities = HashMap::new();

    // TODO: fix these unwraps
    ls.block_on(&rt, async move {
        while let Some((action, idx)) = input.next().await {
            match action {
                StreamYield::Item(WorkerInput::Action(LuaContextAction::NewEntity { script, entity, rx, param })) => {
                    let m: LuaTable = lua.load(&*script).eval().unwrap();
        
                    let e: LuaTable = m.get::<_, LuaTable>("entities")
                        .unwrap()
                        .get(&*entity)
                        .unwrap();

                    let param = lua.to_value(&param).unwrap();
        
                    let state: LuaTable = e.call_function("init", (param,)).unwrap();

                    let sidx = input.insert({
                        rx.pipe(UnboundedReceiverStream::new)
                            .map(WorkerInput::Entity)
                            .pipe(|s| Box::new(s)as Box<dyn Stream<Item = WorkerInput<C>> + Unpin>)
                    });

                    entities.insert(sidx, Entity {
                        spec: e.into_owned(),
                        state: state.into_owned(),
                    });
                },
                StreamYield::Item(WorkerInput::Entity(LuaEntityAction::Rerender(finished))) => {

                },
                StreamYield::Item(WorkerInput::Entity(LuaEntityAction::Handle(message, finished))) => {

                },
                StreamYield::Item(WorkerInput::Entity(LuaEntityAction::Query(query, finished))) => {

                },
                StreamYield::Item(WorkerInput::Entity(LuaEntityAction::Lifecycle(action, finished))) => {

                },
                StreamYield::Finished(f) => {
                    f.remove(Pin::new(&mut input));
                    if idx == action_idx {
                        break;
                    } else {
                        let _ = entities.remove(&idx).unwrap();
                    }
                },
            }
        }
    });
}

impl<C: Context> LuaContext<C> {
    pub fn start(e: Weak<EntityList<C>>) -> Self {
        let (tx, rx) = unbounded_channel();

        std::thread::spawn(move || spawn::<C>(e, rx));

        Self {
            tx
        }
    }
}

pub trait LuaEntityImpl<C: Context>: Send + Sync + std::fmt::Debug {
    const SCRIPT: &'static str;
    const ENTITY_TYPE: &'static str;

    type Param: EntityParameter<C> + serde::Serialize;

    fn get_context(e: &EntityList<C>) -> BoxedResult<Arc<LuaContext<C>>>;
}

#[derive(Debug)]
pub struct LuaEntity<C: Context, I: LuaEntityImpl<C> + 'static> {
    id: Id,
    tx: UnboundedSender<LuaEntityAction<C>>,

    _c: PhantomData<(C, I)>,
}

impl<C: Context, I: LuaEntityImpl<C> + 'static> PureEntity<C> for LuaEntity<C, I> {
    fn new() -> Effect<C, EntityReference<C>> {
        Effect::deferred(|e| {
            let c = I::get_context(e)?;

            let (tx, rx) = unbounded_channel();

            c.tx.send(LuaContextAction::NewEntity {
                script: Cow::Borrowed(I::SCRIPT),
                entity: Cow::Borrowed(I::ENTITY_TYPE),
                rx,
                param: Value::Null,
            }).unwrap();

            Ok(Arc::new(RwLock::new(Self {
                id: Id::new_uuid(),
                tx,

                _c: PhantomData::default(),
            })) as EntityReference<C>)
        })
    }
}

impl<C: Context, I: LuaEntityImpl<C> + 'static> Entity<C> for LuaEntity<C, I> {
    fn get_id(&self) -> Id {
        self.id.clone()
    }

    fn get_type(&self) -> CowStr {
        Cow::Borrowed(I::ENTITY_TYPE)
    }

    fn set_parent(&mut self, parent: Option<Id>) -> Effect<C> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Lifecycle(LuaEntityLifecycle::SetParent(parent), tx))?;

        rx.recv()?
    }

    fn on_added(&mut self) -> Effect<C> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Lifecycle(LuaEntityLifecycle::Added, tx))?;

        rx.recv()?
    }

    fn on_removed(&mut self) -> Effect<C> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Lifecycle(LuaEntityLifecycle::Removed, tx))?;

        rx.recv()?
    }

    fn on_pushed(&mut self) -> Effect<C> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Lifecycle(LuaEntityLifecycle::Pushed, tx))?;

        rx.recv()?
    }

    fn on_popped(&mut self) -> Effect<C> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Lifecycle(LuaEntityLifecycle::Popped, tx))?;

        rx.recv()?
    }

    fn tick(&mut self) -> Effect<C> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Lifecycle(LuaEntityLifecycle::Ticked, tx))?;

        rx.recv()?
    }

    fn handle(&mut self, msg: &mut Message) -> Effect<C> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Handle(msg.clone(), tx))?;

        rx.recv()?
    }

    fn query(&self, query: &mut Query) -> Effect<C, Response> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Query(query.clone(), tx))?;

        rx.recv()?
    }

    fn render(&self) -> BoxedResult<Option<Node>> {
        let (tx, rx) = std::sync::mpsc::channel();

        self.tx.send(LuaEntityAction::Rerender(tx))?;

        Ok(rx.recv()?)
    }
}
