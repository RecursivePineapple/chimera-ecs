
#[macro_export]
macro_rules! name_of {
    ($name:ident in $ty:ty) => {
        {
            #[allow(dead_code)]
            fn dummy(v: $ty) { let _ = &v.$name; }
            stringify!($name)
        }
    };

    ($name:ident) => {
        {
            #[allow(dead_code)]
            fn dummy(_: $name) {}
            stringify!($name)
        }
    };
}

// #[macro_export]
// macro_rules! hash_map {
//     ({ $(,)? }) => {{
//         std::collections::HashMap::new()
//     }};
//     ({$($k:expr => $v:expr),* $(,)? }) => {{
//         let mut m = std::collections::HashMap::new();
//         $(
//             m.insert($k, $v);
//         )*
//         m
//     }};
// }

// #[macro_export]
// macro_rules! key {
//     ([$t:expr]) => (($t).to_string());
//     ($t:expr) => (String::from(stringify!($t)));
// }

// #[macro_export]
// macro_rules! str_hash_map2_tt_muncher {
//     ($m:ident, $(,)?) => {

//     };
//     ($m:ident, $x:tt $(, $($rest:tt)*)?) => {
//         $m.insert(String::from(stringify!($x)), ($x).into());
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
//     ($m:ident, $x:tt if $c:expr $(, $($rest:tt)*)?) => {
//         if $c { $m.insert(String::from(stringify!($x)), ($x).into()); }
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
//     ($m:ident, $x:tt if let $p:pat = $c:expr $(, $($rest:tt)*)?) => {
//         if let $p = $c { $m.insert(String::from(stringify!($x)), ($x).into()); }
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
//     ($m:ident, $k:tt => $v:expr $(, $($rest:tt)*)?) => {
//         $m.insert(utils::key!($k), ($v).into());
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
//     ($m:ident, $k:tt if $c:expr => $v:expr $(, $($rest:tt)*)?) => {
//         if $c { $m.insert(utils::key!($k), ($v).into()); }
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
//     ($m:ident, $k:tt if let $p:pat = $c:expr => $v:expr $(, $($rest:tt)*)?) => {
//         if let $p = $c { $m.insert(utils::key!($k), ($v).into()); }
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
//     ($m:ident, .. $map:expr $(, $($rest:tt)*)?) => {
//         $m.extend($map.into_iter());
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
//     ($m:ident, .. $map:tt if $c:expr $(, $($rest:tt)*)?) => {
//         if $c { $m.extend($map.into_iter()); }
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
//     ($m:ident, .. $map:tt if let $p:pat = $c:expr $(, $($rest:tt)*)?) => {
//         if let $p = $c { $m.extend($map.into_iter()); }
//         utils::str_hash_map2_tt_muncher!($m, $($($rest)*)?);
//     };
// }

// #[macro_export]
// macro_rules! str_hash_map {
//     ({ $(,)? }) => {{
//         std::collections::HashMap::new()
//     }};
//     ({$($toks:tt)*}) => {{
//         let mut m = std::collections::HashMap::new();
//         utils::str_hash_map2_tt_muncher!(m, $($toks)*);
//         m
//     }};
// }

// #[macro_export]
// macro_rules! scene_props {
//     ({ $(,)? }) => ({
//         std::collections::HashMap::<String, serde_json::Value>::new()
//     });
//     ({$($k:tt => $v:expr),* $(,)? }) => ({
//         let mut m = std::collections::HashMap::new();
//         $(
//             m.insert(
//                 String::from(stringify!($k)),
//                 serde_json::to_value($v)
//                     .map_err(|e| format!("error stringifying json value for key {}: {}", stringify!($k), e))
//                     .unwrap()
//             );
//         )*
//         m
//     });
// }
