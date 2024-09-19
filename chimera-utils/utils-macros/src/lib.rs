
#![feature(extend_one)]
#![feature(proc_macro_expand)]

extern crate proc_macro;

// use quote::ToTokens;
use syn::{parse_quote, parse_macro_input};

mod hash_map;
// mod fn_result_explain;

#[proc_macro]
pub fn hash_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(input as hash_map::HashMapContents);

    args.into_token_stream().into()
}

#[proc_macro]
pub fn str_hash_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut args = parse_macro_input!(input as hash_map::HashMapContents);

    args.map_key = Some(Box::new(|e| parse_quote!((#e).to_string())));

    args.into_token_stream().into()
}

// #[proc_macro_attribute]
// pub fn explain(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//     let attr = parse_macro_input!(attr as fn_result_explain::ResultExplainParameter);
//     let input = parse_macro_input!(input as fn_result_explain::ResultExplainInput);

//     let output = attr.transform(input);

//     output.to_token_stream().into()
// }
