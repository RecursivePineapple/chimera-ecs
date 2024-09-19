
pub trait Extract: Default {
    fn extract(&mut self) -> Self;
}

impl<T: Default> Extract for T {
    fn extract(&mut self) -> Self {
        std::mem::take(self)
    }
}

pub trait SplitTuple<A, B> {
    fn split_tuple(self) -> (A, B);
}

impl<A, B> SplitTuple<Vec<A>, Vec<B>> for Vec<(A, B)> {
    fn split_tuple(mut self) -> (Vec<A>, Vec<B>) {
        let mut a = Vec::new();
        let mut b = Vec::new();
        a.reserve(self.len());
        b.reserve(self.len());
        self.drain(..).for_each(|(ia, ib)| {
            a.push(ia);
            b.push(ib);
        });
        (a, b)
    }
}

pub trait InsideOut {
    type Output;
    fn inside_out(self) -> Self::Output;
}

impl<T, E> InsideOut for Option<Result<T, E>> {
    type Output = Result<Option<T>, E>;
    fn inside_out(self) -> Self::Output {
        match self {
            Some(Ok(x)) => Ok(Some(x)),
            Some(Err(e)) => Err(e),
            None => Ok(None),
        }
    }
}

impl<T, E> InsideOut for Result<Option<T>, E> {
    type Output = Option<Result<T, E>>;
    fn inside_out(self) -> Self::Output {
        match self {
            Ok(Some(x)) => Some(Ok(x)),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

pub trait InsideOutWith {
    type Output<R>;
    type Type;
    fn inside_out_with<F: FnOnce(Self::Type)->R, R>(self, f: F) -> Self::Output<R>;
}

impl<T, E> InsideOutWith for Option<Result<T, E>> {
    type Output<R> = Result<Option<R>, E>;
    type Type = T;
    fn inside_out_with<F: FnOnce(Self::Type)->R, R>(self, f: F) -> Self::Output<R> {
        match self {
            Some(Ok(x)) => Ok(Some(f(x))),
            Some(Err(e)) => Err(e),
            None => Ok(None),
        }
    }
}

impl<T, E> InsideOutWith for Result<Option<T>, E> {
    type Output<R> = Option<Result<R, E>>;
    type Type = T;
    fn inside_out_with<F: FnOnce(Self::Type)->R, R>(self, f: F) -> Self::Output<R> {
        match self {
            Ok(Some(x)) => Some(Ok(f(x))),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

pub trait InsideOutTuple1 {
    type Output;
    fn inside_out_tuple_1(self) -> Self::Output;
}

pub trait InsideOutTuple2 {
    type Output;
    fn inside_out_tuple_2(self) -> Self::Output;
}

pub trait InsideOutTuple3 {
    type Output;
    fn inside_out_tuple_3(self) -> Self::Output;
}

impl<T, T2, E> InsideOutTuple1 for (Result<T, E>, T2) {
    type Output = Result<(T, T2), E>;
    fn inside_out_tuple_1(self) -> Self::Output {
        Ok((self.0?, self.1))
    }
}

impl<T, T2, E> InsideOutTuple2 for (T, Result<T2, E>) {
    type Output = Result<(T, T2), E>;
    fn inside_out_tuple_2(self) -> Self::Output {
        Ok((self.0, self.1?))
    }
}

impl<T, T2, E> InsideOutTuple3 for (Result<T, E>, Result<T2, E>) {
    type Output = Result<(T, T2), E>;
    fn inside_out_tuple_3(self) -> Self::Output {
        Ok((self.0?, self.1?))
    }
}

pub trait UniqueSortedExt {
    type Index;
    type Item;
    fn push_unique_sorted(&mut self, item: Self::Item) -> Result<Self::Index, (Self::Index, Self::Item)>;
    fn remove_unique_sorted(&mut self, item: &Self::Item) -> Result<Self::Index, ()>;
}

impl<T: Ord> UniqueSortedExt for Vec<T> {
    type Index = usize;
    type Item = T;

    fn push_unique_sorted(&mut self, item: Self::Item) -> Result<Self::Index, (Self::Index, Self::Item)> {
        match self.binary_search(&item) {
            Ok(i) => {
                Err((i, item))
            },
            Err(i) => {
                self.insert(i, item);
                Ok(i)
            },
        }
    }

    fn remove_unique_sorted(&mut self, item: &Self::Item) -> Result<Self::Index, ()> {
        match self.binary_search(&item) {
            Ok(i) => {
                self.remove(i);
                Ok(i)
            },
            Err(_) => {
                Err(())
            },
        }
    }
}

pub trait PipeExt: Sized {
    fn pipe<F: FnOnce(Self)->R, R>(self, f: F) -> R;

    fn pipe_ref<F: FnOnce(&Self)->R, R>(&self, f: F) -> R;

    fn pipe_mut<F: FnOnce(&mut Self)->R, R>(&mut self, f: F) -> R;
}

impl<T: Sized> PipeExt for T {
    #[inline(always)]
    fn pipe<F: FnOnce(Self)->R, R>(self, f: F) -> R {
        f(self)
    }

    #[inline(always)]
    fn pipe_ref<F: FnOnce(&Self)->R, R>(&self, f: F) -> R {
        f(self)
    }

    #[inline(always)]
    fn pipe_mut<F: FnOnce(&mut Self)->R, R>(&mut self, f: F) -> R {
        f(self)
    }
}
