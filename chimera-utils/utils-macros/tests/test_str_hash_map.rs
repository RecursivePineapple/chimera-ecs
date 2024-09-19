
#![cfg(test)]

use std::collections::HashMap;

use utils_macros::*;

#[test]
fn test_str_hash_map_basic() {
    let mut map1 = HashMap::new();

    map1.insert("5".to_owned(), 6);
    map1.insert("7".to_owned(), 8);
    map1.insert("9".to_owned(), 10);
    map1.insert("11".to_owned(), 12);

    assert_eq!(map1, str_hash_map!({
        5 => 6,
        7 => 8,
        9 => 10,
        11 => 12
    }));
}

#[test]
fn test_str_hash_map_conditional() {
    let mut map1 = HashMap::new();

    map1.insert("5".to_owned(), 6);
    map1.insert("9".to_owned(), 10);
    map1.insert("11".to_owned(), 12);

    struct Foo;
    impl Foo {
        pub fn is_thing() -> bool { true }
    }

    assert_eq!(map1, str_hash_map!({
        5 if true => 6,
        7 if false => 8,
        9 if 3 % 2 == 1 => 10,
        11 if Foo::is_thing() => 12
    }));
}

#[test]
fn test_str_hash_map_basic_splat() {
    let mut map1 = HashMap::new();

    map1.insert("5".to_owned(), 6);
    map1.insert("9".to_owned(), 10);
    map1.insert("11".to_owned(), 12);

    assert_eq!(map1, str_hash_map!({
        ..map1.clone()
    }));
}

#[test]
fn test_str_hash_map_conditional_splat() {
    let mut map1 = HashMap::new();
    let mut map2 = HashMap::new();

    map1.insert("5".to_owned(), 6);
    map1.insert("9".to_owned(), 10);
    map1.insert("11".to_owned(), 12);

    map2.insert("51".to_owned(), 61);
    map2.insert("91".to_owned(), 110);
    map2.insert("111".to_owned(), 112);

    assert_eq!(map1, str_hash_map!({
        ..map1.clone() if true,
        ..map2 if false
    }));
}

#[test]
fn test_str_hash_map_keyless() {
    let mut map1 = HashMap::new();

    map1.insert("x".to_owned(), 6i32);

    let x = 6i32;

    assert_eq!(map1, str_hash_map!({
        x
    }));
}

#[test]
fn test_str_hash_map() {
    let mut map1 = HashMap::new();
    let mut map2 = HashMap::new();

    map1.insert("5".to_owned(), 6);
    map1.insert("7".to_owned(), 8);
    map1.insert("9".to_owned(), 10);
    map1.insert("11".to_owned(), 12);
    map1.insert("x".to_owned(), 14);

    map2.insert("7".to_owned(), 8);
    map2.insert("9".to_owned(), 10);

    let key = 13;

    let x = 14;

    assert_eq!(map1, str_hash_map!({
        5 => 6,
        .. map2 if true,
        11 => 12,
        [key] if false => 14,
        x
    }));
}

#[test]
fn test_if_let() {
    let x = Some(5);
    let y: Option<i32> = None;
    let z: Result<i32, i32> = Ok(10);

    let m: HashMap<String, i32> = str_hash_map!({
        x => 5,
        z => 10
    });

    assert_eq!(m, str_hash_map!({
        x if let Some(i) = x => i,
        y if let Some(i) = y => i,
        z if let Ok(i) = z => i
    }));
}
