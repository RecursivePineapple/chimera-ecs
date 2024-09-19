
#![feature(extend_one)]
#![feature(proc_macro_expand)]

extern crate proc_macro;

use quote::ToTokens;
use syn::{Token, punctuated::Punctuated, parse_quote, Expr, spanned::Spanned};

#[allow(dead_code)]
struct KeywordArg {
    pub arg: syn::Ident,
    pub eq: syn::Token![=],
    pub gt: syn::Token![>],
    pub value: Expr
}

impl syn::parse::Parse for KeywordArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            arg: input.parse()?,
            eq: input.parse()?,
            gt: input.parse()?,
            value: input.parse()?
        })
    }
}

enum Arg {
    Positional(Expr),
    Keyword(KeywordArg)
}

impl syn::parse::Parse for Arg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident) && input.peek2(syn::Token![=]) && input.peek3(syn::Token![>]) {
            Ok(Self::Keyword(input.parse()?))
        } else {
            Ok(Self::Positional(input.parse()?))
        }
    }
}

struct Args {
    pub args: Punctuated<Arg, Token![,]>
}

impl syn::parse::Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let args = Punctuated::<Arg, Token![,]>::parse_terminated(input)?;

        let mut found_kwarg = false;
        let mut kwargs = Vec::<String>::new();
        for arg in args.iter() {
            match arg {
                Arg::Positional(a) => {
                    if found_kwarg {
                        return Err(syn::Error::new(a.span(), "All positional arguments must come before keyword arguments."));
                    }
                },
                Arg::Keyword(a) => {
                    found_kwarg = true;
                    if kwargs.contains(&a.arg.to_string()) {
                        return Err(syn::Error::new(a.arg.span(), format!("Duplicate keyword parameter {}.", a.arg)));
                    }
                    kwargs.push(a.arg.to_string());
                },
            }
        }

        Ok(Self {
            args
        })
    }
}

struct SceneArgs {
    scene: Expr,
    props: Expr,
    meta: Expr,
    children: Expr,
    visibility: Expr,
}

impl TryFrom<Args> for SceneArgs {
    type Error = syn::Error;

    fn try_from(value: Args) -> Result<Self, Self::Error> {
        let mut i = 0;
        let mut values = Vec::<Option<Expr>>::new();
        values.resize(5, None);

        for arg in value.args {
            match arg {
                Arg::Positional(v) => {
                    if i > values.len() {
                        return Err(syn::Error::new(v.span(), "Unexpected positional parameter."));
                    } else {
                        values[i] = Some(v);
                        i += 1;
                    }
                },
                Arg::Keyword(KeywordArg { arg, value, .. }) => {
                    match arg.to_string().as_str() {
                        "scene" => {
                            values[0] = Some(value);
                        },
                        "props" => {
                            values[1] = Some(value);
                        },
                        "meta" => {
                            values[2] = Some(value);
                        },
                        "children" => {
                            values[3] = Some(value);
                        },
                        "visibility" => {
                            values[4] = Some(value);
                        }
                        x => {
                            return Err(syn::Error::new(arg.span(), format!("Unknown keyword parameter {x}.")));
                        }
                    }
                },
            };
        }

        Ok(Self {
            scene: values[0].take()
                .ok_or_else(|| syn::Error::new(
                    proc_macro::Span::call_site().into(),
                    "Required parameter 'scene' was not defined."
                ))?,
            props: values[1].take()
                .unwrap_or_else(|| parse_quote!{ std::collections::HashMap::new() }),
            meta: values[2].take()
                .unwrap_or_else(|| parse_quote!{ std::collections::HashMap::new() }),
            children: values[3].take()
                .unwrap_or_else(|| parse_quote!{ chimera_core::node::NodeChildren::None }),
            visibility: values[4].take()
                .map(|p| parse_quote!{ Some(#p) })
                .unwrap_or_else(|| parse_quote!{ None }),
        })
    }
}

#[proc_macro]
pub fn scene(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = syn::parse_macro_input!(input as Args);

    let SceneArgs { scene, props, meta, children, visibility } = match SceneArgs::try_from(args) {
        Ok(a) => a,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };

    let call: Expr = parse_quote!{
        {
            let scene = String::from(#scene);
            let props: std::collections::HashMap<String, chimera_core::node::SceneProp> = (#props);
            let meta: std::collections::HashMap<String, serde_json::Value> = (#meta);
            let children: chimera_core::node::NodeChildren = (#children).into();
            let visibility = (#visibility);

            chimera_core::node::scene(
                ctx,
                &scene,
                &props,
                &meta,
                &children,
                &visibility,
            )
        }
    };

    call.to_token_stream().into()
}
