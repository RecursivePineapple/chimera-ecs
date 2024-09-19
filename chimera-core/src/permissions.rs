use chimera_utils::UniqueSortedExt;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Hash, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ClientId {
    Id(Uuid),
    Singleton(String), // TODO: fragment id
}

impl ClientId {
    pub fn allow_only(self) -> PermissionSet {
        PermissionSet::allow_only(self)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PermissionSet {
    WhiteList(Vec<ClientId>),
    BlackList(Vec<ClientId>),
}

impl PermissionSet {
    pub fn allow_all() -> Self {
        Self::BlackList(Vec::new())
    }

    pub fn allow_only(who: ClientId) -> Self {
        Self::WhiteList(vec![who])
    }

    pub fn deny_only(who: ClientId) -> Self {
        Self::BlackList(vec![who])
    }

    pub fn deny_all() -> Self {
        Self::WhiteList(Vec::new())
    }

    pub fn allow(allow: bool) -> Self {
        if allow {
            Self::allow_all()
        } else {
            Self::deny_all()
        }
    }

    pub fn inverse(self) -> Self {
        match self {
            PermissionSet::WhiteList(x) => PermissionSet::BlackList(x),
            PermissionSet::BlackList(x) => PermissionSet::WhiteList(x),
        }
    }

    pub fn whitelist(iter: impl Iterator<Item = ClientId>) -> Self {
        Self::WhiteList(iter.sorted().collect_vec())
    }

    pub fn blacklist(iter: impl Iterator<Item = ClientId>) -> Self {
        Self::BlackList(iter.sorted().collect_vec())
    }

    pub fn includes(&self, who: Option<&ClientId>) -> bool {
        match self {
            PermissionSet::WhiteList(w) => match who {
                Some(who) => w.contains(who),
                None => false,
            },
            PermissionSet::BlackList(b) => match who {
                Some(who) => !b.contains(who),
                None => true,
            },
        }
    }

    pub fn insert(&mut self, who: ClientId) {
        match self {
            PermissionSet::WhiteList(v) | PermissionSet::BlackList(v) => {
                let _ = v.push_unique_sorted(who);
            }
        }
    }

    pub fn remove(&mut self, who: &ClientId) {
        match self {
            PermissionSet::WhiteList(v) | PermissionSet::BlackList(v) => {
                let _ = v.remove_unique_sorted(who);
            }
        }
    }
}
