use std::{
    backtrace::Backtrace,
    error::Error,
    fmt::{Debug, Display},
    sync::{Arc, Weak},
};

use thiserror::Error;

use crate::{BoxedError, BoxedResult};

#[derive(Error, Debug)]
#[error("could not upgrade weak {weak_name} reference: strong reference was dropped.")]
pub struct UpgradeError {
    pub weak_name: String,
}

impl UpgradeError {
    pub fn new(weak_name: impl Into<String>) -> Self {
        Self {
            weak_name: weak_name.into(),
        }
    }
}

pub trait MustUpgradeExt {
    type Output;

    fn upgrade_or_err(&self, weak_name: impl Into<String>) -> Self::Output;
}

impl<T> MustUpgradeExt for Weak<T>
where
    T: ?Sized,
{
    type Output = Result<Arc<T>, BoxedError>;

    fn upgrade_or_err(&self, weak_name: impl Into<String>) -> Self::Output {
        match self.upgrade() {
            Some(x) => Ok(x),
            None => Err(Box::new(UpgradeError::new(weak_name))),
        }
    }
}

#[derive(Debug, Error)]
#[error("{explain}: {error}")]
pub struct ErrorExplanation<E: Display + Send + Debug> {
    pub explain: String,
    pub error: E,
}

impl<E: Display + Send + Debug> ErrorExplanation<E> {
    pub fn new(explain: impl Into<String>, error: E) -> Self {
        Self {
            explain: explain.into(),
            error,
        }
    }
}

pub trait ExplainExt
where
    Self: Sized + Display + Send + Debug,
{
    fn explain(self, e: impl Into<String>) -> ErrorExplanation<Self>;
}

impl<E: Sized + Display + Send + Debug> ExplainExt for E {
    fn explain(self, e: impl Into<String>) -> ErrorExplanation<Self> {
        ErrorExplanation::new(e, self)
    }
}

pub trait MapExplainExt
where
    Self: Sized,
{
    type Output;

    fn map_err_explain(self, e: impl Into<String>) -> Self::Output;
    fn map_err_explain_with(self, f: impl FnOnce() -> String) -> Self::Output;
}

impl<T, E: Display + Send + Debug> MapExplainExt for Result<T, E> {
    type Output = Result<T, ErrorExplanation<E>>;

    fn map_err_explain(self, msg: impl Into<String>) -> Self::Output {
        self.map_err(|e| ErrorExplanation::new(msg, e))
    }

    fn map_err_explain_with(self, f: impl FnOnce() -> String) -> Self::Output {
        self.map_err(|e| ErrorExplanation::new((f)(), e))
    }
}

pub enum Explainer<'a> {
    Message(String),
    Closure(Box<dyn FnOnce() -> String + Send + Sync + 'a>),
}

impl<'a> Explainer<'a> {
    pub fn explain<T, E: Error + Send + Sync>(
        self,
        res: Result<T, E>,
    ) -> Result<T, ErrorExplanation<E>> {
        match self {
            Explainer::Message(m) => res.map_err_explain(m),
            Explainer::Closure(c) => res.map_err_explain_with(c),
        }
    }
}

impl From<String> for Explainer<'static> {
    fn from(value: String) -> Self {
        Explainer::Message(value)
    }
}

impl From<&str> for Explainer<'static> {
    fn from(value: &str) -> Self {
        Explainer::Message(value.to_owned())
    }
}

impl<'a, F: FnOnce() -> String + Send + Sync + 'a> From<F> for Explainer<'a> {
    fn from(value: F) -> Self {
        Explainer::Closure(Box::new(value))
    }
}

pub trait MapBoxErr {
    type Output;
    fn map_err_boxed(self) -> Self::Output;
}

impl<T, E: Error + Send + Sync + 'static> MapBoxErr for Result<T, E> {
    type Output = BoxedResult<T>;

    fn map_err_boxed(self) -> Self::Output {
        self.map_err(|e| BoxedError::from(e))
    }
}

#[derive(Debug)]
pub struct VecError<E: Sized + Display + Send + Debug> {
    pub errors: Vec<E>,
}

impl<E: Sized + Display + Send + Debug> VecError<E> {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }
}

impl<E: Sized + Display + Send + Debug> From<Vec<E>> for VecError<E> {
    fn from(errors: Vec<E>) -> Self {
        Self { errors }
    }
}

impl<E: Sized + Display + Send + Debug> Error for VecError<E> {}

impl<E: Sized + Display + Send + Debug> Display for VecError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("one or more errors occurred:")?;

        for (i, e) in self.errors.iter().enumerate() {
            f.write_fmt(format_args!(
                "{}: {}{}",
                i + 1,
                e,
                if i > 0 { "," } else { "" }
            ))?;
        }

        Ok(())
    }
}

pub trait StrToBoxErrorExt {
    fn to_boxed_error(&self) -> BoxedError;
}

impl<'a> StrToBoxErrorExt for &'a str {
    fn to_boxed_error(&self) -> BoxedError {
        <Box<dyn Error + Sync + Send>>::from(*self) as BoxedError
    }
}

impl StrToBoxErrorExt for String {
    fn to_boxed_error(&self) -> BoxedError {
        Box::new(ErrorWithBacktrace {
            error: (<Box<dyn Error + Sync + Send>>::from(self.as_str()) as BoxedError),
            backtrace: Backtrace::force_capture(),
        }) as BoxedError
    }
}

pub trait ErrorIntoStringExt {
    type Output;

    fn map_err_into_string(self) -> Self::Output;
}

impl<'a, T, E: Error + 'a> ErrorIntoStringExt for Result<T, E> {
    type Output = BoxedResult<T>;
    fn map_err_into_string(self) -> Self::Output {
        self.map_err(|e| e.to_string().to_boxed_error())
    }
}

pub trait MapErrWithBacktrace {
    type Output;
    fn map_err_with_backtrace(self) -> Self::Output;
}

#[derive(Error, Debug)]
#[error("{error}\n\nBacktrace:\n{backtrace}")]
pub struct ErrorWithBacktrace<E: Display + Debug + Send + Sync + 'static> {
    pub error: E,
    #[backtrace]
    pub backtrace: Backtrace,
}

impl<T, E: Display + Debug + Send + Sync + 'static> MapErrWithBacktrace for Result<T, E> {
    type Output = BoxedResult<T>;

    fn map_err_with_backtrace(self) -> Self::Output {
        self.map_err(|e| {
            Box::new(ErrorWithBacktrace {
                error: e,
                backtrace: Backtrace::force_capture(),
            }) as BoxedError
        })
    }
}
