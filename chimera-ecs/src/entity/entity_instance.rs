
use std::sync::atomic::{AtomicBool, Ordering};

use derivative::Derivative;

use crate::prelude::*;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct EntityInstanceData {
    pub entity_type: String,
    pub data: Value
}

impl EntityInstanceData {
    pub(crate) fn save<C: Context>(e: &EntityReference<C>) -> BoxedResult<Option<Self>> {
        let e = read_lock_with_timeout(e)?;
        
        let entity_type = e.get_type().to_string();
        let data = match e.save()? {
            None => {
                return Ok(None);
            },
            Some(x) => x
        };

        Ok(Some(Self {
            entity_type,
            data
        }))
    }

    // pub(crate) fn load<E: CreatableEntity>(self, e: &EntityList) -> BoxedResult<EntityReference> {
    //     if self.entity_type != E::TYPE {
    //         return Err(format!("unexpected entity type {}: expected {}", self.entity_type, E::TYPE).to_boxed_error());
    //     }

    //     Ok(E::load(self.data).evaluate(e)?)
    // }

    pub(crate) fn load_generic<C: Context>(self, e: &EntityList<C>, f: &EntityCreator<C>) -> BoxedResult<EntityReference<C>> {
        let e = match f(e, &self.entity_type, Some(self.data)) {
            Some(e) => e,
            None => {
                return Err(format!("entity factory could not load entity of type '{}'", self.entity_type).to_boxed_error());
            }
        };

        Ok(e)
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct EntityInstance<C: Context> {
    pub(crate) id: Id,
    pub(crate) entity_type: CowStr,

    pub(crate) parent: Option<Id>,
    pub(crate) children: HashSet<Id>,

    #[derivative(Debug = "ignore")]
    pub(crate) entity: EntityRef<C>,


    #[derivative(Debug = "ignore")]
    pub(crate) cache: Option<Option<Node>>,
    pub(crate) dirty: AtomicBool,
}

impl<C: Context> EntityInstance<C> {
    pub(crate) fn new(parent: Option<Id>, entity: EntityRef<C>) -> BoxedResult<Self> {
        let e_l = entity.try_deref().map_err_explain("could not deref entity ref")?;
        let l = read_lock_with_timeout(&e_l)?;

        let id = l.get_id();
        let entity_type = l.get_type();

        drop(l);

        Ok(Self {
            id,
            entity_type,
            parent,
            children: HashSet::new(),
            entity,
            cache: None,
            dirty: AtomicBool::new(true),
        })
    }

    pub fn render(&mut self) -> BoxedResult<Option<Node>> {
        if !self.dirty.load(Ordering::Relaxed) && let Some(c) = self.cache.clone() {
            return Ok(c);
        }

        let f = self.try_deref()?.with_mut_explain(
            |e| {
                e.render().map_err_explain_with(|| format!("could not render entity {}", self.id))
            },
            || format!("could not lock entity {}", self.id)
        )??;

        self.dirty.store(false, Ordering::Relaxed);
        self.cache = Some(f.clone());

        Ok(f)
    }

    /// Intentionally gated behind a read lock, so that it won't have a race condition with `Self::render`.
    pub(crate) fn mark_dirty(&self) {
        self.dirty.store(true, Ordering::Relaxed);
    }

    pub fn get_entity_type(&self) -> &str {
        &self.entity_type
    }

    pub fn get_id(&self) -> &Id {
        &self.id
    }

    pub fn get_parent(&self) -> Option<&Id> {
        self.parent.as_ref()
    }

    pub fn get_children(&self) -> &HashSet<Id> {
        &self.children
    }

    pub unsafe fn try_get_entity(&self) -> BoxedResult<EntityReference<C>> {
        self.try_deref()
    }

    pub unsafe fn with_entity<R>(&self, f: impl FnOnce(&mut dyn Entity<C>)->R) -> BoxedResult<R> {
        self.try_deref()?.with_mut(|e| {
            (f)(e)
        })
    }

    pub(crate) fn try_deref(&self) -> BoxedResult<EntityReference<C>> {
        Ok(self.entity.try_deref().map_err_explain_with(|| format!("could not deref entity instance {}", self.id))?)
    }
}
