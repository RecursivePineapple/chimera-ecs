
use proc_macro2::{TokenStream, Span};
use quote::{ToTokens, quote, quote_spanned};
use syn::{*, parse::Parse, spanned::Spanned, punctuated::Punctuated};

pub(crate) enum Key {
    Indexed(Expr),
    Ident(Ident),
    Literal(syn::Lit),
}

impl Parse for Key {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Bracket) {
            let content;
            bracketed!(content in input);
            Ok(Self::Indexed(content.parse()?))
        } else if input.peek(syn::Ident) {
            Ok(Self::Ident(input.parse()?))
        } else {
            Ok(Self::Literal(input.parse()?))
        }
    }
}

impl Key {
    pub fn as_key_expr(&self) -> Expr {
        match self {
            Key::Indexed(e) => {
                parse_quote!(#e)
            },
            Key::Ident(i) => {
                let s = i.to_string();
                parse_quote!(#s)
            },
            Key::Literal(l) => {
                parse_quote!(#l)
            }
        }
    }

    pub fn as_value_expr(&self) -> Expr {
        match self {
            Key::Indexed(e) => {
                e.clone()
            },
            Key::Ident(i) => {
                parse_quote!(#i)
            },
            Key::Literal(l) => {
                parse_quote!(#l)
            }
        }
    }
}

pub(crate) struct KeyValue {
    pub span: Span,
    pub key: Key,
    pub cond: Option<(Token![if], Expr)>,
    #[allow(dead_code)]
    pub fat_arrow: Option<(Token![=], Token![>])>,
    pub value: Expr,
}

impl Parse for KeyValue {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let key: Key = input.parse()?;
        let cond = if input.peek(Token![if]) {
            Some((input.parse()?, input.parse()?))
        } else {
            None
        };

        let (fat_arrow, value) = if input.peek(Token![,]) || input.is_empty() {
            (None, key.as_value_expr())
        } else {
            (Some((input.parse()?, input.parse()?)), input.parse()?)
        };

        Ok(Self {
            span: key.as_key_expr().span().join(value.span()).unwrap_or(value.span()),
            key,
            cond,
            fat_arrow,
            value,
        })
    }
}

pub(crate) struct Extend {
    pub span: Span,
    #[allow(dead_code)]
    pub dots: Token![..],
    pub map: Expr,
    pub cond: Option<(Token![if], Expr)>,
}

impl Parse for Extend {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let dots = input.parse()?;
        let map: Expr = input.parse()?;
        let cond = if input.peek(Token![if]) {
            Some((input.parse()?, input.parse()?))
        } else {
            None
        };

        Ok(Self {
            span: map.span(),
            dots,
            map,
            cond
        })
    }
}

pub(crate) enum HashMapItem {
    KeyValue(KeyValue),
    Extend(Extend),
}

impl Parse for HashMapItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![..]) {
            Ok(Self::Extend(input.parse()?))
        } else {
            Ok(Self::KeyValue(input.parse()?))
        }
    }
}

pub(crate) type KeyMapFn = Option<Box<dyn Fn(Expr)->Expr>>;

pub(crate) type ValueMapFn = Option<Box<dyn Fn(Expr)->Expr>>;

impl HashMapItem {
    pub fn into_token_stream(self, map_key: &KeyMapFn, map_value: &ValueMapFn) -> TokenStream {
        match self {
            HashMapItem::KeyValue(KeyValue { span, key, cond, fat_arrow: _, value }) => {
            
                let key = key.as_key_expr();
                let key = match map_key {
                    Some(f) => (f)(key),
                    None => key,
                };

                let value = match map_value {
                    Some(f) => (f)(value),
                    None => value,
                };

                match cond {
                    Some((_, x)) => {
                        quote_spanned!(span => if #x { __m.insert(#key, #value); })
                    },
                    None => {
                        quote_spanned!(span => __m.insert(#key, #value);)
                    }
                }
            },
            HashMapItem::Extend(Extend { span, dots: _, map, cond }) => {
                let extend = match (map_key, map_value) {
                    (None, None) => quote_spanned!(span => (#map).into_iter()),
                    (map_key, map_value) => {

                        let key: Expr = parse_quote_spanned!(span => k);
                        let key = match map_key {
                            Some(map_key) => (map_key)(key),
                            None => key,
                        };

                        let value: Expr = parse_quote_spanned!(span => v);
                        let value = match map_value {
                            Some(map_value) => (map_value)(value),
                            None => value,
                        };

                        quote_spanned!(span => (#map).into_iter().map(|(k, v)| (#key, #value)))
                    },
                };

                match cond {
                    Some((_, x)) => {
                        quote_spanned!(span => if #x { __m.extend(#extend); })
                    },
                    None => {
                        quote_spanned!(span => __m.extend(#extend);)
                    }
                }
            },
        }
    }
}

pub(crate) struct HashMapContents {
    #[allow(dead_code)]
    pub brace: syn::token::Brace,
    pub p: Punctuated<HashMapItem, Token![,]>,
    pub map_key: KeyMapFn,
    pub map_value: ValueMapFn,
}

impl Parse for HashMapContents {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content; 
        Ok(Self {
            brace: braced!(content in input),
            p: Punctuated::parse_terminated(&content)?,
            map_key: None,
            map_value: None
        })
    }
}

impl HashMapContents {
    pub fn into_token_stream(self) -> TokenStream {

        let inserts = self.p.into_pairs()
            .map(|x| x.into_value().into_token_stream(&self.map_key, &self.map_value))
            .reduce(|a, b| quote!(#a #b));

        let expr: Expr = match inserts {
            Some(inserts) => {
                parse_quote_spanned!(Span::call_site() => {
                    let mut __m = std::collections::HashMap::new();
                    #inserts
                    __m
                })
            },
            None => {
                parse_quote_spanned!(Span::call_site() =>std::collections::HashMap::new())
            }
        };

        expr.into_token_stream()
    }
}
