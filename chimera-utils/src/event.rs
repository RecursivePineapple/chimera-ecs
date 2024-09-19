use std::{collections::LinkedList, sync::Arc, fmt::Debug};

use crate::{Mutex, lock_with_timeout, BoxedResult, MaybeStrong, MapExplainExt, VecError};

pub type Callback<T> = dyn Fn(&T)->BoxedResult<()> + Sync + Send + 'static;

pub trait Watchable<T> where T: Send, Self: Sync + Send {
    type WatchGuard;
    fn watch(&self, f: Box<Callback<T>>) -> BoxedResult<Self::WatchGuard>;
    fn watch_permanent(&self, f: Box<Callback<T>>) -> BoxedResult<()>;
}

pub trait Invokable<T> where T: Send + 'static, Self: Sync + Send {
    fn invoke(&self, value: &T) -> BoxedResult<()>;
}

pub struct Subscription<T>(#[allow(dead_code)] Arc<Callback<T>>);

impl<T> Debug for Subscription<T> where T: Send {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Subscription").finish_non_exhaustive()
    }
}

#[derive(Clone)]
pub struct BroadcastEvent<T> where T: Send + 'static, Self: Sync + Send {
    subs: Arc<Mutex<LinkedList<MaybeStrong<Callback<T>>>>>
}

impl<T> Debug for BroadcastEvent<T> where T: Send {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BroadcastEvent").finish_non_exhaustive()
    }
}

impl<T> BroadcastEvent<T> where T: Send, Self: Sync + Send {
    pub fn new() -> Self {
        Self { subs: Arc::new(Mutex::new(LinkedList::new())) }
    }

    pub fn gc_subscriptions(&self) -> BoxedResult<()> {
        let mut s = lock_with_timeout(&self.subs)?;

        s.extract_if(|w| w.try_deref().is_none())
            .for_each(drop);

        Ok(())
    }
}

impl<T> Watchable<T> for BroadcastEvent<T> where T: Sync + Send + 'static, Self: Sync + Send {
    type WatchGuard = Subscription<T>;

    fn watch(&self, f: Box<Callback<T>>) -> BoxedResult<Self::WatchGuard> {
        let f = Arc::new(f) as Arc<Callback<T>>;

        lock_with_timeout(&self.subs)?.push_back(MaybeStrong::Weak(Arc::downgrade(&f)));

        Ok(Subscription(f))
    }

    fn watch_permanent(&self, f: Box<Callback<T>>) -> BoxedResult<()> {
        let f = Arc::new(f) as Arc<Callback<T>>;

        lock_with_timeout(&self.subs)?.push_back(MaybeStrong::Strong(f));

        Ok(())
    }
}

impl<T> Invokable<T> for BroadcastEvent<T> where T: Send + 'static, Self: Sync + Send {
    fn invoke(&self, value: &T) -> BoxedResult<()> {
        let mut s = lock_with_timeout(&self.subs).map_err_explain("could not lock subs mutex")?;

        let mut errs = Vec::new();

        s.extract_if(|w| {
                match w.try_deref() {
                    Some(f) => {
                        match (f)(&value) {
                            Ok(_) => false,
                            Err(e) => {
                                errs.push(e);
                                true
                            }
                        }
                    },
                    None => {
                        true
                    },
                }
            })
            .for_each(drop);

        if errs.is_empty() {
            Ok(())
        } else {
            Err(VecError::from(errs).into())
        }
    }
}

#[derive(Clone)]
pub struct Delegate<T> where T: Send, Self: Sync + Send {
    f: Arc<Box<Callback<T>>>,
}

impl<T> Debug for Delegate<T> where T: Send {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Delegate").finish_non_exhaustive()
    }
}

impl<T> Delegate<T> where T: Send, Self: Sync + Send {
    pub fn new(f: Box<Callback<T>>) -> Self {
        Self { f: Arc::new(f) }
    }
}

impl<T> Invokable<T> for Delegate<T> where T: Send + 'static, Self: Sync + Send {
    fn invoke(&self, value: &T) -> BoxedResult<()> {
        (self.f)(value)
    }
}

pub type SubscriptionWatchRef<'a, T> = &'a dyn Watchable<T, WatchGuard = Subscription<T>>;
pub type DynInvokeRef<'a, T> = &'a dyn Invokable<T>;
