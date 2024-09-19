use async_trait::async_trait;
use futures::future::BoxFuture;
use serde_json::Value;
use chimera_utils::BoxedResult;
use uuid::Uuid;


pub trait EntityStorage {
    type Handle: EntityStorageHandle;

    fn start() -> (Self::Handle, BoxFuture<'static, BoxedResult<()>>);
}

#[async_trait]
pub trait EntityStorageHandle {
    async fn load(&self, id: Uuid) -> BoxedResult<Option<Value>>;
    async fn save(&self, id: Uuid, value: Option<Value>) -> BoxedResult<()>;
}
