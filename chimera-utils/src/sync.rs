
use std::{time::Duration, sync::TryLockError, backtrace::Backtrace};

#[cfg(feature = "no_deadlocks")]
pub use no_deadlocks::{
    RwLock,
    RwLockWriteGuard,
    RwLockReadGuard,
    Mutex,
    MutexGuard,
};

#[cfg(not(feature = "no_deadlocks"))]
pub use std::sync::{
    RwLock,
    RwLockWriteGuard,
    RwLockReadGuard,
    Mutex,
    MutexGuard,
};

use crate::{BoxedResult, Explainer};

const N_TRIES: i32 = 1000;
const D_PER_ITER: Duration = Duration::from_micros(1);

#[derive(thiserror::Error, Debug)]
pub enum LockError {
    #[error("lock was poisoned: {0}\nBacktrace:\n{1}")]
    Poisoned(String, #[backtrace] Backtrace),
    #[error("lock timed out\nBacktrace:\n{0}")]
    Timeout(#[backtrace] Backtrace)
}

pub fn read_lock_with_timeout<T: ?Sized>(m: &RwLock<T>) -> Result<RwLockReadGuard<'_, T>, LockError> {
    let mut i = 0;
    
    loop {
        match m.try_read() {
            Ok(l) => {
                return Ok(l);
            },
            Err(TryLockError::Poisoned(e)) => {
                return Err(LockError::Poisoned(e.to_string(), Backtrace::force_capture()));
            },
            Err(TryLockError::WouldBlock) => {
                if i == N_TRIES {
                    return Err(LockError::Timeout(Backtrace::force_capture()));
                } else {
                    std::thread::sleep(D_PER_ITER);
                    i += 1;
                }
            },
        }
    }
}

pub fn write_lock_with_timeout<T: ?Sized>(m: &RwLock<T>) -> Result<RwLockWriteGuard<'_, T>, LockError> {
    let mut i = 0;
    
    loop {
        match m.try_write() {
            Ok(l) => {
                return Ok(l);
            },
            Err(TryLockError::Poisoned(e)) => {
                return Err(LockError::Poisoned(e.to_string(), Backtrace::force_capture()));
            },
            Err(TryLockError::WouldBlock) => {
                if i == N_TRIES {
                    return Err(LockError::Timeout(Backtrace::force_capture()));
                } else {
                    std::thread::sleep(D_PER_ITER);
                    i += 1;
                }
            },
        }
    }
}

pub fn lock_with_timeout<T: ?Sized>(m: &Mutex<T>) -> Result<MutexGuard<'_, T>, LockError> {
    let mut i = 0;
    
    loop {
        match m.try_lock() {
            Ok(l) => {
                return Ok(l);
            },
            Err(TryLockError::Poisoned(e)) => {
                return Err(LockError::Poisoned(e.to_string(), Backtrace::force_capture()));
            },
            Err(TryLockError::WouldBlock) => {
                if i == N_TRIES {
                    return Err(LockError::Timeout(Backtrace::force_capture()));
                } else {
                    std::thread::sleep(D_PER_ITER);
                    i += 1;
                }
            },
        }
    }
}

pub trait WithImmutLock {
    type Value: ?Sized;

    fn with_immut<R, F: FnOnce(&Self::Value)->R>(self, f: F) -> BoxedResult<R>;
    fn with_immut_explain<'a, R, F: FnOnce(&Self::Value)->R>(self, f: F, explain: impl Into<Explainer<'a>>) -> BoxedResult<R>;
}

pub trait WithMutLock {
    type Value: ?Sized;

    fn with_mut<R, F: FnOnce(&mut Self::Value)->R>(self, f: F) -> BoxedResult<R>;
    fn with_mut_explain<'a, R, F: FnOnce(&mut Self::Value)->R>(self, f: F, explain: impl Into<Explainer<'a>>) -> BoxedResult<R>;
}

impl<T: ?Sized> WithImmutLock for &RwLock<T> {
    type Value = T;

    fn with_immut<R, F: FnOnce(&Self::Value)->R>(self, f: F) -> BoxedResult<R> {
        let guard = read_lock_with_timeout(self)?;

        Ok(f(&*guard))
    }

    fn with_immut_explain<'a, R, F: FnOnce(&Self::Value)->R>(self, f: F, explain: impl Into<Explainer<'a>>) -> BoxedResult<R> {
        let explain: Explainer = explain.into();

        let guard = explain.explain(read_lock_with_timeout(self))?;

        Ok(f(&*guard))
    }
}

impl<T: ?Sized> WithMutLock for &RwLock<T> {
    type Value = T;

    fn with_mut<R, F: FnOnce(&mut Self::Value)->R>(self, f: F) -> BoxedResult<R> {
        let mut guard = write_lock_with_timeout(self)?;

        Ok(f(&mut *guard))
    }

    fn with_mut_explain<'a, R, F: FnOnce(&mut Self::Value)->R>(self, f: F, explain: impl Into<Explainer<'a>>) -> BoxedResult<R> {
        let explain: Explainer = explain.into();

        let mut guard = explain.explain(write_lock_with_timeout(self))?;

        Ok(f(&mut *guard))
    }
}

impl<T: ?Sized> WithMutLock for &Mutex<T> {
    type Value = T;

    fn with_mut<R, F: FnOnce(&mut Self::Value)->R>(self, f: F) -> BoxedResult<R> {
        let mut guard = lock_with_timeout(self)?;

        Ok(f(&mut *guard))
    }

    fn with_mut_explain<'a, R, F: FnOnce(&mut Self::Value)->R>(self, f: F, explain: impl Into<Explainer<'a>>) -> BoxedResult<R> {
        let explain: Explainer = explain.into();

        let mut guard = explain.explain(lock_with_timeout(self))?;

        Ok(f(&mut *guard))
    }
}
