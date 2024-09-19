use std::{marker::PhantomData, cell::Cell, sync::MutexGuard, error::Error, borrow::Cow};

pub type PhantomUnsync = PhantomData<Cell<()>>;
pub type PhantomUnsend = PhantomData<MutexGuard<'static, ()>>;
pub type BoxedError = Box<dyn Error + Send + Sync>;
pub type BoxedResult<T = ()> = Result<T, BoxedError>;
pub type CowStr = Cow<'static, str>;
