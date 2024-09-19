pub trait Context: std::fmt::Debug + Send + Sync + 'static {}

impl<T: std::fmt::Debug + Send + Sync + 'static> Context for T {}

#[cfg(feature = "coroutines")]
mod coroutines {
    use crate::prelude::*;
    use std::{
        marker::PhantomData,
        ops::{Coroutine, CoroutineState},
        pin::Pin,
    };

    mod sealed {
        pub trait Sealed {}
    }

    #[must_use = "effects will not run unless you use the coroutine."]
    pub trait HandleCoroutine<R = (), C: Context = ()>:
        Coroutine<(), Yield = Effect<C>, Return = BoxedResult<R>> + Unpin + sealed::Sealed
    {
        #[doc(hidden)]
        fn __coerce_return(&self, r: R) -> R {
            r
        }
    }

    impl<T, R, C: Context> HandleCoroutine<R, C> for T where
        T: Coroutine<(), Yield = Effect<C>, Return = BoxedResult<R>> + Unpin
    {
    }

    impl<T, R, C: Context> sealed::Sealed for T where
        T: Coroutine<(), Yield = Effect<C>, Return = BoxedResult<R>> + Unpin
    {
    }

    pub trait IterableHandleCoroutine<R> {
        type Iter;

        fn into_iterator(self) -> Self::Iter;
    }

    impl<T: HandleCoroutine<R>, R> IterableHandleCoroutine<R> for T {
        type Iter = HandleCoroutineIterator<T, R>;

        fn into_iterator(self) -> Self::Iter {
            HandleCoroutineIterator(Some(self), Default::default())
        }
    }

    pub struct HandleCoroutineIterator<T: HandleCoroutine<R>, R>(Option<T>, PhantomData<R>);

    impl<T: HandleCoroutine<R>, R> Iterator for HandleCoroutineIterator<T, R> {
        type Item = BoxedResult<Effect<()>>;

        fn next(&mut self) -> Option<Self::Item> {
            match self.0.as_mut().map(|g| Pin::new(g).resume(()))? {
                CoroutineState::Yielded(e) => Some(Ok(e)),
                CoroutineState::Complete(r) => {
                    let _ = self.0.take();
                    match r {
                        Ok(_) => None,
                        Err(e) => Some(Err(e)),
                    }
                }
            }
        }
    }

    #[inline]
    pub fn consume_coroutine<C: Context, H: HandleCoroutine<(), C>>(
        l: &mut EffectList<C>,
        mut g: H,
    ) {
        loop {
            let s = Pin::new(&mut g).resume(());

            match s {
                CoroutineState::Yielded(e) => {
                    l.push(e);
                }
                CoroutineState::Complete(r) => {
                    if let Err(e) = r {
                        l.errs.push(e);
                    }
                    break;
                }
            }
        }
    }

    #[inline]
    pub fn from_coroutine<C: Context, H: HandleCoroutine<(), C>>(g: H) -> Effect<C> {
        let mut l = EffectList::new();

        consume_coroutine(&mut l, g);

        l.into_effect()
    }

    #[macro_export]
    macro_rules! yield_all {
        ($e:expr, $g:expr) => {{
            let mut g = ($g);

            use chimera_ecs::prelude::HandleCoroutine;
            let ret = loop {
                let s = std::pin::Pin::new(&mut g as &mut dyn HandleCoroutine<_>).resume(());

                match s {
                    std::ops::CoroutineState::Yielded(e) => {
                        yield e;
                    }
                    std::ops::CoroutineState::Complete(r) => {
                        break r?;
                    }
                }
            };

            g.__coerce_return(ret)
        }};
    }

    #[macro_export]
    macro_rules! yield_all_vec {
        ($g:expr) => {{
            let mut g = ($g);
            let mut v = Vec::<chimera_ecs::prelude::Effect>::new();

            use chimera_ecs::prelude::HandleCoroutine;
            let ret = loop {
                let s = std::pin::Pin::new(&mut g as &mut dyn HandleCoroutine<_>).resume(());

                match s {
                    std::ops::CoroutineState::Yielded(e) => {
                        v.push(e);
                    }
                    std::ops::CoroutineState::Complete(r) => {
                        break r?;
                    }
                }
            };

            let ret = g.__coerce_return(ret);

            drop(g);

            for e in v {
                yield e;
            }

            ret
        }};
    }
}

#[cfg(feature = "coroutines")]
pub use coroutines::{
    from_coroutine, HandleCoroutine, HandleCoroutineIterator, IterableHandleCoroutine,
};
