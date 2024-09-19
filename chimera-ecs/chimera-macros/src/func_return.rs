
extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::format_ident;
use syn::{
    parse_quote, parse_quote_spanned,
    spanned::Spanned, Expr, Ident, Type, TypePath, TypeParamBound, PathArguments, AngleBracketedGenericArguments, GenericArgument, Path, ReturnType,
};

use crate::util::*;

#[derive(Debug, PartialEq)]
pub enum ActionReturn {
    Nil,
    #[cfg(feature = "coroutines")]
    Generator,
    Message,
    Effect,
    Result(Box<ActionReturn>),
    Option(Box<ActionReturn>),
    Expr(Expr),
}

#[must_use]
pub enum ActionReturnBase {
    OptionEffect,
    Nil,
}

enum Ret<'a> {
    Tuple(Vec<Ret<'a>>),
    Option(Box<Ret<'a>>),
    Result(Box<Ret<'a>>),
    Effect(Box<Ret<'a>>),
    Generator,
    Message,
    Unknown(&'a Type)
}

impl<'a> TryFrom<&'a Path> for Ret<'a> {
    type Error = TokenStream;

    fn try_from(path: &'a Path) -> Result<Self, TokenStream> {
        let ident = path.segments.first().map(|p| &p.ident).unwrap();

        match ident.to_string().as_str() {
            "Option" => {

            },
            "Result" => {

            },
            "BoxedResult" => {

            },
            "Effect" => {

            },
            "HandleCoroutine" => {

            },
            "Message" => {

            },
            "Option" => {

            },
        }

        todo!()
    }
}

impl<'a> TryFrom<&'a Type> for Ret<'a> {
    type Error = TokenStream;

    fn try_from(ret: &'a Type) -> Result<Self, TokenStream> {
        match ret {
            Type::Paren(inner) => Ret::try_from(inner.elem.as_ref()),
            Type::Path(t) => Ret::try_from(&t.path),

            Type::ImplTrait(x) => {
                Ok(Ret::Tuple(
                    x.bounds.iter()
                        .filter_map(|b| match b {
                            TypeParamBound::Trait(t) => {
                                let i = t.path.get_ident();
                                if let Some(i) = i && (i == "Send" || i == "Sync" || i == "Debug" || i == "Any") {
                                    None
                                } else {
                                    Some(Ret::try_from(&t.path))
                                }
                            },
                            TypeParamBound::Lifetime(_) => None,
                        })
                        .collect::<Result<Vec<_>, TokenStream>>()?
                ))
            },

            Type::Tuple(tuple) => {
                Ok(Ret::Tuple(
                    tuple.elems.iter()
                        .map(|e| Ret::try_from(e))
                        .collect::<Result<Vec<_>, TokenStream>>()?
                ))
            },

            other => {
                Ok(Ret::Unknown(other))
            },
        }
    }
}

impl ActionReturn {
    pub fn try_from(ret: &Type) -> Result<Self, TokenStream> {
        match ret {
            #[cfg(feature = "coroutines")]
            Type::ImplTrait(i) => {

                let t = i.bounds.iter()
                    .filter_map(|x| match x {
                        TypeParamBound::Lifetime(_) => None,
                        TypeParamBound::Trait(t) => Some(t)
                    })
                    .collect::<Vec<_>>();

                if t.len() != 1 {
                    return Err(to_compile_error(i.span(), "expected only one impl trait bound (HandleCoroutine)"));
                }

                let t = t[0];

                if !t.path.is_ident("HandleCoroutine") {
                    return Err(to_compile_error(t.span(), "expected type: HandleCoroutine"));
                }

                Ok(Self::Generator)
            },
            Type::Path(TypePath { qself: None, path }) => {

                let first = path.segments
                    .first()
                    .ok_or_else(|| to_compile_error(path.span(), "expected path"))?;

                match first.ident.to_string().as_str() {
                    "Message" => {
                        Ok(Self::Message)
                    },
                    "Effect" => {
                        Ok(Self::Effect)
                    },
                    "HandleResult" => {
                        Ok(Self::Result(Box::new(Self::Option(Box::new(Self::Effect)))))
                    },
                    "Option" => {
                        let args = match &first.arguments {
                            PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                                match args.first() {
                                    Some(GenericArgument::Type(t)) => t,
                                    Some(other) => {
                                        return Err(to_compile_error(other.span(), "expected type argument"));
                                    },
                                    None => {
                                        return Err(to_compile_error(first.arguments.span(), "expected one and only one argument"));
                                    },
                                }
                            },
                            _ => {
                                return Err(to_compile_error(first.span(), "expected one and only one argument"));
                            }
                        };
    
                        Ok(Self::Option(Box::new(ActionReturn::try_from(args)?)))
                    },
                    "BoxedResult" => {
                        let args = match &first.arguments {
                            PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                                match args.first() {
                                    Some(GenericArgument::Type(t)) => t.clone(),
                                    Some(other) => {
                                        return Err(to_compile_error(other.span(), "expected type argument"));
                                    },
                                    None => {
                                        return Err(to_compile_error(first.span(), "expected one and only one argument"));
                                    },
                                }
                            },
                            _ => {
                                Type::Tuple(parse_quote!(()))
                            }
                        };
    
                        Ok(Self::Result(Box::new(ActionReturn::try_from(&args)?)))
                    },
                    _ => {
                        Err(to_compile_error(ret.span(),
                            "illegal return type: must match this recursive type EXACTLY: X = HandleResult | BoxedResult<X> | Option<X> | impl HandleCoroutine | Message | Effect<...> | Effect | ()"
                        ))
                    }
                }
            },
            Type::Tuple(t) => {
                if t.elems.len() == 0 {
                    Ok(Self::Nil)
                } else {
                    Err(to_compile_error(ret.span(),
                        "expected empty tuple"
                    ))
                }
            },
            _ => {
                Err(to_compile_error(ret.span(),
                    "illegal return type: must match this recursive type EXACTLY: X = HandleResult | BoxedResult<X> | Option<X> | impl HandleCoroutine | Message | Effect<()> | Effect | ()"
                ))
            },
        }
    }

    pub fn get_base(&self) -> ActionReturnBase {
        match self {
            ActionReturn::Nil => ActionReturnBase::Nil,
            #[cfg(feature = "coroutines")]
            ActionReturn::Generator => ActionReturnBase::OptionEffect,
            ActionReturn::Message => ActionReturnBase::OptionEffect,
            ActionReturn::Effect => ActionReturnBase::OptionEffect,
            ActionReturn::Result(x) => x.get_base(),
            ActionReturn::Option(x) => x.get_base(),
            ActionReturn::Expr(_) => ActionReturnBase::OptionEffect,
        }
    }

    pub fn transform(self, span: Span, e: Expr) -> Expr {
        match self {
            ActionReturn::Nil => {
                e
            },
            #[cfg(feature = "coroutines")]
            ActionReturn::Generator => {
                parse_quote_spanned!(span=>
                    ::chimera_ecs::prelude::from_coroutine(#e)?
                )
            },
            ActionReturn::Message => {
                parse_quote_spanned!(span=>
                    Some(::chimera_ecs::prelude::effects::send(#e))
                )
            },
            ActionReturn::Effect => {
                parse_quote_spanned!(span=>
                    Some(#e)
                )
            },
            ActionReturn::Result(r) => {
                let e = parse_quote_spanned!(span=>
                    (#e?)
                );
                r.transform(span.clone(), e)
            },
            ActionReturn::Option(o) => {
                let e = parse_quote_spanned!(span=>
                    match (#e) {
                        Some(e) => e,
                        None => {
                            return Ok(None);
                        }
                    }
                );
                o.transform(span.clone(), e)
            },
            ActionReturn::Expr(e) => {
                e
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum QueryReturn {
    #[cfg(feature = "coroutines")]
    Generator(Box<QueryReturn>),
    Effect(Box<QueryReturn>),
    Result(Box<QueryReturn>),
    Rest(Type),
    Value,
    Expr(Expr, Option<Type>),
}

fn unwrap_generic(p: &Path) -> Option<(&Ident, Option<&Type>)> {
    if p.segments.len() == 1 {
        if let PathArguments::AngleBracketed(generics) = &p.segments[0].arguments && generics.args.len() == 1 &&
           let GenericArgument::Type(next) = &generics.args[0] {
            Some((&p.segments[0].ident, Some(next)))
        } else {
            Some((&p.segments[0].ident, None))
        }
    } else {
        None
    }
}

impl QueryReturn {
    pub fn try_from_ret(ret: &ReturnType) -> Result<Self, TokenStream> {
        match &ret {
            ReturnType::Default => {
                Err(to_compile_error(ret.span(), "query functions must return something"))
            },
            ReturnType::Type(_, box t) => {
                Ok(Self::try_from(t)?)
            },
        }
    }

    pub fn try_from(ret: &Type) -> Result<Self, TokenStream> {
        match ret {
            #[cfg(feature = "coroutines")]
            Type::ImplTrait(i) => {
                let t = i.bounds.iter()
                    .filter_map(|x| match x {
                        TypeParamBound::Lifetime(_) => None,
                        TypeParamBound::Trait(t) => Some(t)
                    })
                    .collect::<Vec<_>>();

                if t.len() > 1 {
                    return Err(to_compile_error(t[1].span(), "expected only one impl trait bound (HandleCoroutine)"));
                }

                let t = t[0];

                match unwrap_generic(&t.path) {
                    None | Some((_, None)) => {
                        Err(to_compile_error(t.path.span(), "expected type 'HandleCoroutine' with single generic parameter"))
                    },
                    Some((ident, Some(next))) => {
                        if ident == "HandleCoroutine" {
                            Ok(Self::Generator(Box::new(QueryReturn::try_from(next)?)))
                        } else {
                            Err(to_compile_error(t.path.span(), "expected type 'HandleCoroutine' with single generic parameter"))
                        }
                    }
                }
            },
            Type::Path(TypePath { qself: None, path }) => {
                if path.is_ident("Value") {
                    return Ok(Self::Value);
                }

                let (ident, next) = match unwrap_generic(&path) {
                    None => {
                        return Err(to_compile_error(ret.span(),
                            "illegal return type: must match this recursive type EXACTLY: X = BoxedResult<X> | Result<X, impl Error> | impl HandleCoroutine<X> | Effect<X> | impl serde::Serialize"
                        ));
                    },
                    Some((_, None)) => {
                        return Ok(Self::Rest(ret.clone()));
                    },
                    Some((ident, next)) => {
                        (ident, next.unwrap())
                    }
                };

                if ident == "Result" || ident == "BoxedResult" {
                    return Ok(Self::Result(Box::new(QueryReturn::try_from(next)?)));
                }
                
                if ident == "Effect" {
                    return Ok(Self::Effect(Box::new(QueryReturn::try_from(next)?)));
                }
                
                Ok(Self::Rest(ret.clone()))
            },
            _ => {
                Err(to_compile_error(ret.span(),
                    "illegal return type: must match this recursive type EXACTLY: X = BoxedResult<X> | Result<X, impl Error> | impl HandleCoroutine<X> | Effect<X> | impl serde::Serialize"
                ))
            },
        }
    }

    pub fn get_bounds(&self) -> Option<TokenStream> {
        match self {
            #[cfg(feature = "coroutines")]
            QueryReturn::Generator(e) => {
                e.get_bounds()
            },
            QueryReturn::Effect(e) |
            QueryReturn::Result(e) => {
                e.get_bounds()
            },
            QueryReturn::Rest(t) => {
                Some(quote::quote_spanned! {t.span()=>
                    #t: serde::Serialize
                })
            },
            QueryReturn::Value => {
                None
            },
            QueryReturn::Expr(_, _) => {
                None
            },
        }
    }

    pub fn is_async(&self) -> bool {
        match self {
            #[cfg(feature = "coroutines")]
            QueryReturn::Generator(_) => true,
            QueryReturn::Effect(_) => true,
            QueryReturn::Result(x) => x.is_async(),
            QueryReturn::Rest(_) => false,
            QueryReturn::Value => false,
            QueryReturn::Expr(_, _) => false,
        }
    }

    pub fn get_base_type(&self) -> Option<syn::Type> {
        match self {
            #[cfg(feature = "coroutines")]
            QueryReturn::Generator(x) => x.get_base_type(),
            QueryReturn::Effect(x) => x.get_base_type(),
            QueryReturn::Result(x) => x.get_base_type(),
            QueryReturn::Rest(t) => Some(t.clone()),
            QueryReturn::Value => {
                Some(parse_quote!(Value))
            },
            QueryReturn::Expr(_, t) => t.clone(),
        }
    }

    pub fn transform(self, span: Span, e: Expr) -> Expr {
        match self {
            #[cfg(feature = "coroutines")]
            QueryReturn::Generator(inner) => {
                let e: Expr = parse_quote_spanned! {span=>
                    {
                        let mut g = (#e);
        
                        use ::chimera_ecs::prelude::HandleCoroutine;
                        let ret = loop {
                            let s = std::pin::Pin::new(&mut g as &mut dyn HandleCoroutine<_>).resume(());
        
                            match s {
                                std::ops::GeneratorState::Yielded(e) => {
                                    e.evaluate(__chimera_entity_list)?;
                                },
                                std::ops::GeneratorState::Complete(r) => {
                                    break r?;
                                },
                            }
                        };
        
                        g.__coerce_return(ret)
                    }
                };
                
                inner.transform(span, e)
            },
            QueryReturn::Effect(inner) => {
                let e: Expr = parse_quote_spanned! {span=>
                    (#e).evaluate(__chimera_entity_list)?
                };

                inner.transform(span, e)
            },
            QueryReturn::Result(inner) => {
                let e: Expr = parse_quote_spanned! {span=>
                    (#e)?
                };

                inner.transform(span, e)
            },
            QueryReturn::Rest(_) => {
                parse_quote_spanned! {span=>
                    ::serde_json::to_value(#e)?
                }
            },
            QueryReturn::Value => {
                parse_quote_spanned! {span=>
                    ((#e) as ::serde_json::Value)
                }
            },
            QueryReturn::Expr(e, _) => e,
        }
    }
}
