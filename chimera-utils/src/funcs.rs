use std::{collections::HashMap, hash::Hash, fmt::Debug};

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub enum VecDeltaChange<V> {
    Left(V),
    Right(V),
}

pub fn delta_vecs<'a, T: PartialEq + Clone>(left: &'a [T], right: &'a [T]) -> Vec<VecDeltaChange<T>> {

    let l = left.iter().filter(|x| !right.contains(x)).cloned().map(VecDeltaChange::Left);
    let r = right.iter().filter(|x| !left.contains(x)).cloned().map(VecDeltaChange::Right);

    l.chain(r).collect::<Vec<_>>()
}

#[derive(Debug, Clone, PartialEq)]
pub enum HashMapDeltaChange<VL, VR> {
    Left(VL),
    Changed(VL, VR),
    Right(VR),
}

pub fn delta_hash_maps<'a, K: Hash + Eq + Ord, VL: PartialEq<VR>, VR>(left: &'a HashMap<K, VL>, right: &'a HashMap<K, VR>) ->
            Vec<(&'a K, HashMapDeltaChange<&'a VL, &'a VR>)> {
    left.keys().chain(right.keys())
        .sorted()
        .dedup()
        .filter_map(|k| {
            match (left.get(k), right.get(k)) {
                (Some(l), Some(r)) if l != r => Some((k, HashMapDeltaChange::Changed(l, r))),
                (None, Some(r)) => Some((k, HashMapDeltaChange::Right(r))),
                (Some(l), None) => Some((k, HashMapDeltaChange::Left(l))),
                _ => None
            }
        })
        .collect()
}

pub fn delta_hash_maps2<K: Hash + Eq + Clone, VL: PartialEq<VR>, VR>(left: &HashMap<K, VL>, right: &HashMap<K, VR>) -> Vec<(K, HashMapDeltaChange<(), ()>)> {
    left.keys().chain(right.keys())
        .unique()
        .filter_map(|k| {
            match (left.get(k), right.get(k)) {
                (Some(l), Some(r)) if l != r => Some((k.clone(), HashMapDeltaChange::Changed((), ()))),
                (None, Some(_)) => Some((k.clone(), HashMapDeltaChange::Right(()))),
                (Some(_), None) => Some((k.clone(), HashMapDeltaChange::Left(()))),
                _ => None
            }
        })
        .collect()
}

pub trait TryPartialEq<Rhs: ?Sized = Self> {
    type Error;

    fn try_eq(&self, rhs: &Rhs) -> Result<bool, Self::Error>;

}

impl<L: PartialEq<R>, R> TryPartialEq<R> for L {
    type Error = !;

    fn try_eq(&self, rhs: &R) -> Result<bool, Self::Error> {
        Ok(self == rhs)
    }
}

#[cfg(test)]
mod tests {
    use utils_macros::hash_map;

    use super::*;

    #[test]
    fn test_delta_vecs() {
        assert_eq!(delta_vecs::<i32>(&[], &[]), vec![]);

        assert_eq!(delta_vecs(&[1], &[]), vec![VecDeltaChange::Left(1)]);
        assert_eq!(delta_vecs(&[], &[1]), vec![VecDeltaChange::Right(1)]);
        assert_eq!(delta_vecs(&[1], &[1]), vec![]);

        assert_eq!(delta_vecs(&[1], &[2]), vec![VecDeltaChange::Left(1), VecDeltaChange::Right(2)]);
        assert_eq!(delta_vecs(&[1, 2], &[2, 1]), vec![]);
    }

    #[test]
    fn test_delta_hash_maps() {
        assert_eq!({
            delta_hash_maps::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({}))
        }, vec![(&1, HashMapDeltaChange::Left(&1))]);

        assert_eq!({
            delta_hash_maps::<i32, i32, i32>(&hash_map!({}), &hash_map!({1 => 1}))
        }, vec![(&1, HashMapDeltaChange::Right(&1))]);

        assert_eq!({
            delta_hash_maps::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({1 => 1}))
        }, vec![]);

        assert_eq!({
            delta_hash_maps::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({1 => 2}))
        }, vec![(&1, HashMapDeltaChange::Changed(&1, &2))]);

        assert_eq!({
            delta_hash_maps::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({2 => 1}))
        }, {
            vec![(&1, HashMapDeltaChange::Left(&1)), (&2, HashMapDeltaChange::Right(&1))]
        });

        assert_eq!({
            delta_hash_maps::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({2 => 2}))
        }, {
            vec![(&1, HashMapDeltaChange::Left(&1)), (&2, HashMapDeltaChange::Right(&2))]
        });
    }

    #[test]
    fn test_delta_hash_maps2() {
        assert_eq!({
            delta_hash_maps2::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({}))
        }, vec![(1, HashMapDeltaChange::Left(()))]);

        assert_eq!({
            delta_hash_maps2::<i32, i32, i32>(&hash_map!({}), &hash_map!({1 => 1}))
        }, vec![(1, HashMapDeltaChange::Right(()))]);

        assert_eq!({
            delta_hash_maps2::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({1 => 1}))
        }, vec![]);

        assert_eq!({
            delta_hash_maps2::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({1 => 2}))
        }, vec![(1, HashMapDeltaChange::Changed((), ()))]);

        assert_eq!({
            delta_hash_maps2::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({2 => 1}))
        }, {
            vec![(1, HashMapDeltaChange::Left(())), (2, HashMapDeltaChange::Right(()))]
        });

        assert_eq!({
            delta_hash_maps2::<i32, i32, i32>(&hash_map!({1 => 1}), &hash_map!({2 => 2}))
        }, {
            vec![(1, HashMapDeltaChange::Left(())), (2, HashMapDeltaChange::Right(()))]
        });
    }
}
