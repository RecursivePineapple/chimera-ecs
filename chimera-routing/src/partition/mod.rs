use std::{fmt::Debug, ops::Range};

use uuid::Uuid;


pub trait Partitionable: Sized + Clone + Debug + PartialEq {
    fn all() -> Range<Self>;

    fn split(range: Range<Self>, at: Self) -> (Range<Self>, Range<Self>);
}

impl Partitionable for Uuid {
    fn all() -> Range<Self> {
        Range {
            start: Uuid::from_u128(u128::MIN),
            end: Uuid::from_u128(u128::MAX),
        }
    }

    fn split(range: Range<Self>, at: Self) -> (Range<Self>, Range<Self>) {
        (Range {
            start: range.start,
            end: at.clone()
        }, Range {
            start: at,
            end: range.end
        })
    }
}


