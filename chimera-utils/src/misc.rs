use std::{sync::{Arc, Weak}, ops::Deref};

#[derive(Debug, Clone)]
pub enum MaybeStrong<T: ?Sized> {
    Strong(Arc<T>),
    Weak(Weak<T>),
}

impl<T: ?Sized> MaybeStrong<T> {
    pub fn try_deref(&self) -> Option<Arc<T>> {
        match self {
            MaybeStrong::Strong(a) => Some(Arc::clone(a)),
            MaybeStrong::Weak(w) => Weak::upgrade(w),
        }
    }
}

pub enum MaybeHeap<T: Sized> {
    Heap(Arc<T>),
    Stack(T)
}

impl<T> Deref for MaybeHeap<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeHeap::Heap(t) => Arc::as_ref(t),
            MaybeHeap::Stack(t) => t,
        }
    }
}
