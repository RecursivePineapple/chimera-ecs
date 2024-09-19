
#![feature(trait_alias)]
#![feature(error_generic_member_access)]
#![feature(result_flattening)]
#![feature(let_chains)]
#![feature(iterator_try_collect)]
#![feature(try_trait_v2)]
#![feature(trait_upcasting)]

#![cfg_attr(test, feature(test))]

#![cfg_attr(feature = "coroutines", feature(coroutines, coroutine_trait))]

pub mod entity;
pub mod utils;
pub mod prelude;

pub extern crate chimera_core;
