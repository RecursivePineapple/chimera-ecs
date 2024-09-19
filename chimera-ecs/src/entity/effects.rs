
use std::{marker::PhantomData, ops::{FromResidual, Try}, convert::Infallible};

use derivative::Derivative;
use serde::de::DeserializeOwned;

use crate::prelude::*;

pub mod effects {
    use super::*;

    pub fn send<C: Context>(msg: Message) -> Effect<C> {
        Effect::deferred(move |c| {
            c.send(msg).map(|_| ())
        })
    }

    pub fn query<C: Context>(query: Query) -> Effect<C, Response> {
        let query: Query = query.into();

        Effect::deferred(move |c| {
            let id = query.dest.clone();

            c.query(query)?.unpaused_or_else(|| format!("could not send query to paused entity {id}").to_boxed_error())
        })
    }

    pub fn query_typed<R: DeserializeOwned + Send + 'static, C: Context>(query: Query) -> Effect<C, R> {
        let query: Query = query.into();

        Effect::deferred(move |c| {
            let id = query.dest.clone();

            let resp = c.query(query)?;
    
            let r = resp.try_convert::<R>(|| format!("could not send query to paused entity {id}").to_boxed_error())?;
            
            Ok(r)
        })
    }

    pub fn with_paused_render<R: Send + 'static, C: Context>(e: Effect<C, R>) -> Effect<C, R> {
        Effect::deferred(move |c| {
            c.with_paused_render(move || {
                e.evaluate(c)
            }).flatten()
        })
    }

    pub fn push_entity<C: Context>(parent: Option<Id>, e: EntityReference<C>, is_add: bool) -> Effect<C> {
        Effect::deferred(move |c| {
            c.push_entity(parent, EntityRef::Owned(e), is_add).map(|_| ())
        })
    }

    pub fn push_new_entity<E: Entity<C> + PureEntity<C>, C: Context>(parent: Option<Id>) -> Effect<C> {
        Effect::deferred(move |c| {
            c.add_new_entity::<E>(parent).map(|_| ())
        })
    }

    pub fn pop_entity<C: Context>(id: Id, is_remove: bool) -> Effect<C> {
        Effect::deferred(move |c| {
            c.pop_entity(&id, is_remove).map(|_| ())
        })
    }

    pub fn atomic_pop_push<C: Context>(pop: Id, is_remove: bool, parent: Option<Id>, push: EntityRef<C>, is_add: bool) -> Effect<C> {
        Effect::deferred(move |c| {
            c.atomic_pop_push(pop, is_remove, parent, push, is_add).map(|_| ())
        })
    }

    pub fn pause<C: Context>(id: Id) -> Effect<C> {
        Effect::deferred(move |c| {
            c.pause(&id).map(|_| ())
        })
    }

    pub fn resume<C: Context>(id: Id) -> Effect<C> {
        Effect::deferred(move |c| {
            c.resume(&id).map(|_| ())
        })
    }

    pub fn mark_dirty<C: Context>(id: Id) -> Effect<C> {
        Effect::deferred(move |c| {
            c.mark_dirty(&id)
        })
    }

    pub fn sequential<C: Context>(effects: Vec<Effect<C>>) -> Effect<C> {
        Effect::deferred(move |c| {
            for effect in effects {
                effect.evaluate(c)?;
            }
            Ok(())
        })
    }

    pub fn set_should_tick<C: Context>(id: Id, should_tick: bool) -> Effect<C> {
        Effect::deferred(move |c| {
            c.set_should_tick(&id, should_tick)
        })
    }

    pub fn noop<T: Default, C: Context>() -> Effect<C, T> {
        Effect::Value(T::default(), PhantomData::default())
    }
}

#[must_use = "Effects must be evaluated or returned."]
#[derive(Derivative)]
#[derivative(Debug)]
pub enum Effect<C: Context = (), T = ()> {
    None(T, PhantomData<C>),
    Err(BoxedError, PhantomData<C>),
    Deferred(#[derivative(Debug = "ignore")] Box<dyn FnOnce(&EntityList<C>)->BoxedResult<T> + Send + 'static>, PhantomData<C>),
    Value(T, PhantomData<C>)
}

impl<C: Context, T, E: Into<BoxedError>> FromResidual<Result<Infallible, E>> for Effect<C, T> {
    #[inline]
    fn from_residual(residual: Result<Infallible, E>) -> Self {
        Self::Err(residual.unwrap_err().into(), PhantomData::default())
    }
}

impl<C: Context, T: Default> FromResidual<Option<Infallible>> for Effect<C, T> {
    #[inline]
    fn from_residual(_: Option<Infallible>) -> Self {
        Self::None(T::default(), PhantomData::default())
    }
}

impl<C: Context, T> Try for Effect<C, T> {
    type Output = Effect<C, T>;

    type Residual = Result<Infallible, BoxedError>;

    fn from_output(output: Self::Output) -> Self {
        output
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Effect::Err(e, _) => std::ops::ControlFlow::Break(Err(e)),
            other => std::ops::ControlFlow::Continue(other)
        }
    }
}

pub trait EffectValue {
    type Context: Context;
    type Value;
}

impl<C: Context, T> EffectValue for Effect<C, T> {
    type Context = C;
    type Value = T;
}

impl<C: Context, T: Send + 'static> Effect<C, T> {
    #[inline]
    pub fn evaluate(self, e: &EntityList<C>) -> BoxedResult<T> {
        match self {
            Self::None(x, _) => Ok(x),
            Self::Err(e, _) => Err(e),
            Self::Deferred(f, _) => (f)(e),
            Self::Value(x, _) => Ok(x),
        }
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None(_, _))
    }

    #[inline]
    pub fn into_result(self) -> BoxedResult<Self> {
        match self {
            Self::Err(e, _) => Err(e),
            other => Ok(other),
        }
    }

    #[inline]
    pub fn none() -> Self where T: Default {
        Self::None(T::default(), PhantomData::default())
    }

    #[inline]
    pub fn deferred<F: FnOnce(&EntityList<C>)->BoxedResult<T> + Send + 'static>(f: F) -> Self {
        Self::Deferred(Box::new(f), Default::default())
    }

    #[inline]
    pub fn value(t: T) -> Self {
        Self::Value(t, PhantomData::default())
    }

    #[inline]
    pub fn map<T2: Send + 'static, F: FnOnce(T)->T2 + Send + 'static>(self, f: F) -> Effect<C, T2> {
        Effect::deferred(move |e| {
            Ok((f)(self.evaluate(e)?))
        })
    }

    #[inline]
    pub fn and_then<T2: Send + 'static, F: FnOnce(T)->Effect<C, T2> + Send + 'static>(self, next: F) -> Effect<C, T2> {
        Effect::deferred(|e| {
            let x = self.evaluate(e)?;

            let x2 = (next)(x).evaluate(e)?;

            Ok(x2)
        })
    }

    #[inline]
    pub fn or_catch(self, catch: Effect<C, T>) -> Effect<C, T> {
        Effect::deferred(|c| {
            match self.evaluate(c) {
                Ok(r) => Ok(r),
                Err(e) => {
                    match catch.evaluate(c) {
                        Ok(_) => Err(e),
                        Err(e2) => {
                            Err(VecError::from(vec![
                                e,
                                e2
                            ]).into())
                        }
                    }
                }
            }
        })
    }

    #[inline]
    pub fn ignore_error<T2: Default + Sync + Send + 'static>(self) -> Effect<C, T2> {
        Effect::deferred(move |e| {
            let _ = self.evaluate(e);
            Ok(T2::default())
        })
    }
}

impl<C: Context> Effect<C, Value> {
    pub fn evaluate_deserialize<R: DeserializeOwned>(self, c: &EntityList<C>) -> BoxedResult<R> {
        let value = self.evaluate(c)?;

        Ok(serde_json::from_value(value)?)
    }
}

impl<C: Context, T: Default> Default for Effect<C, T> {
    #[inline]
    fn default() -> Self {
        Self::Value(T::default(), PhantomData::default())
    }
}

pub struct EffectList<C: Context = ()> {
    pub errs: Vec<BoxedError>,
    pub effects: Vec<Effect<C, ()>>,
}

impl<C: Context> EffectList<C> {
    pub fn new() -> Self {
        Self {
            errs: Vec::new(),
            effects: Vec::new()
        }
    }

    #[inline]
    pub fn push(&mut self, effect: Effect<C>) -> &mut Self {
        match effect {
            Effect::None(_, _) => { },
            Effect::Err(e, _) => { self.errs.push(e); },
            e @ Effect::Deferred(_, _) => { self.effects.push(e) },
            Effect::Value(_, _) => { },
        }

        self
    }

    pub fn evaluate(self, l: &EntityList<C>) -> BoxedResult {
        if self.errs.len() > 0 {
            return Err(Box::new(VecError::from(self.errs).explain("error in effect list")));
        }

        for e in self.effects {
            e.evaluate(l)?;
        }

        Ok(())
    }

    #[inline]
    pub fn into_effect(self) -> Effect<C> {
        if self.errs.len() > 0 {
            return Effect::Err(Box::new(VecError::from(self.errs).explain("error in effect list")), PhantomData::default());
        }

        if self.effects.is_empty() {
            return Effect::None((), PhantomData::default());
        }

        Effect::deferred(|e| {
            self.evaluate(e)
        })
    }
}
