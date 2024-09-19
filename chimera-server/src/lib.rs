
use std::{sync::{Arc, atomic::{AtomicBool, Ordering}, Condvar}, time::{Duration, Instant}, marker::PhantomData, pin::Pin, collections::HashMap};
use chimera_ecs::prelude::*;
use chimera_core::{node::{Node, PassiveNodeSource, ActiveNodeSource}};
use futures::Future;
use tokio::{sync::Notify, select};
use tokio_util::sync::CancellationToken;
use chimera_utils::*;

pub struct EntityListRenderer {
    list: Arc<EntityList>,
    root: Box<dyn Fn(HashMap<Id, Node>)->Node + Send + Sync>,
}

impl EntityListRenderer {
    pub fn new(list: Arc<EntityList>, root_scene: String) -> BoxedResult<Self> {
        Self::new_with_root(list.clone(), Box::new(move |e| Self::render(e, &root_scene)))
    }

    fn render(entities: HashMap<Id, Node>, root_scene: &str) -> Node {
        Node {
            scene: root_scene.to_owned(),
            children: entities.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            ..Default::default()
        }
    }

    pub fn new_with_root(list: Arc<EntityList>, root: Box<dyn Fn(HashMap<Id, Node>)->Node + Send + Sync>) -> BoxedResult<Self> {
        Ok(Self {
            list,
            root,
        })
    }
}

impl PassiveNodeSource for EntityListRenderer {
    fn get_tree(&self) -> BoxedResult<Option<Arc<Node>>> {
        let entities = self.list.render().map_err_explain("could not render entity list")?;
        Ok(Some(Arc::new((self.root)(entities))))
    }

    fn on_changed(&self) -> SubscriptionWatchRef<'_, ()> {
        self.list.on_request_rerender()
    }
}

pub async fn entity_list_ticker(list: Arc<EntityList>, period: Duration) -> BoxedResult<()> {
    loop {
        let start = Instant::now();

        list.tick()?;

        let end = start + period;
        let n = Instant::now();

        if end < n {
            tracing::warn!(what = "EntityList ticker is overloaded", amount = ?(n - end));
        } else {
            tokio::time::sleep_until((start + period).into()).await;
        }
    }
}

#[allow(dead_code)]
pub struct ThreadNodePump<S: PassiveNodeSource + Send + 'static> {
    thread: Option<(std::thread::JoinHandle<BoxedResult<()>>, Arc<Condvar>, Arc<AtomicBool>)>,
    push: BroadcastEvent<Option<Arc<Node>>>,
    guard: Subscription<()>,
    _s: PhantomData<S>,
}

#[allow(dead_code)]
pub struct AsyncNodePump<S: PassiveNodeSource + Send + 'static> {
    cancel: CancellationToken,
    push: BroadcastEvent<Option<Arc<Node>>>,
    guard: Subscription<()>,
    _s: PhantomData<S>,
}

impl<S: PassiveNodeSource + Send + 'static> AsyncNodePump<S> {
    pub fn new(source: S) -> BoxedResult<(Self, Pin<Box<impl Future<Output = BoxedResult<()>>>>)> {
        let push = BroadcastEvent::new();
        let cancel = tokio_util::sync::CancellationToken::new();
        let notify = Arc::new(Notify::new());
        let changed = Arc::new(AtomicBool::new(false));

        let guard = source.on_changed().watch(Box::new({
            let notify = notify.clone();
            let changed = changed.clone();
            move |_| {
                tracing::debug!(what = "pump notify callback was called");
                changed.store(true, Ordering::Relaxed);
                notify.notify_waiters();

                Ok(())
            }
        }))?;

        let fut = Box::pin({
            let push = push.clone();
            let cancel = cancel.clone();

            async move {
                let cancel = cancel.cancelled();
                tokio::pin!(cancel);
    
                loop {
                    while let Ok(true) = changed.compare_exchange(true, false, Ordering::Relaxed, Ordering::Relaxed) {
                        let tree = source.get_tree()?; // TODO: only log errors?

                        push.invoke(&tree)?;
                    }

                    select! {
                        _ = notify.notified() => {
                            tracing::debug!(what = "pump task has received notify");
                        },
                        _ = cancel.as_mut() => {
                            tracing::info!(what = "pump task was cancelled");
                            break Ok(());
                        }
                    }
                }
            }
        });

        Ok((
            Self {
                cancel,
                push,
                guard,
                _s: PhantomData::default(),
            },
            fut
        ))
    }

    pub fn cancel(&mut self) -> BoxedResult<()> {
        self.cancel.cancel();

        Ok(())
    }
}

impl<S: PassiveNodeSource + Send + 'static> ActiveNodeSource for AsyncNodePump<S> {
    fn on_push(&self) -> SubscriptionWatchRef<'_, Option<Arc<Node>>> {
        &self.push
    }
}
