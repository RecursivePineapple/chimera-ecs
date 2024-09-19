#![feature(extend_one)]
#![feature(proc_macro_expand)]
#![feature(let_chains)]
#![feature(box_patterns)]
#![feature(iterator_try_collect)]

extern crate proc_macro;

use std::collections::HashMap;

use chimera_utils::*;
use convert_case::Casing;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    meta::ParseNestedMeta,
    parse::Parse,
    parse_macro_input, parse_quote, parse_quote_spanned,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Brace, FatArrow},
    Arm, Attribute, Expr, ExprMethodCall, FieldMutability, Ident, ImplItem, ImplItemFn, ItemImpl,
    LitStr, ReturnType, Token, Type,
};

#[allow(dead_code)]
mod util;
use util::*;

enum Callback {
    Command(FuncAttribute),
    Query(FuncAttribute),
    Action(FuncAttribute),
}

impl ToTokens for Callback {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Callback::Command(a) => a.to_tokens(tokens),
            Callback::Query(a) => a.to_tokens(tokens),
            Callback::Action(a) => a.to_tokens(tokens),
        }
    }
}

const COMMAND_ATTR_NAME: &str = "command";
const QUERY_ATTR_NAME: &str = "query";
const ACTION_ATTR_NAME: &str = "action";

const MESSAGE_ATTR_NAME: &str = "message";
const FROM_ATTR_NAME: &str = "from";
const NEXT_ATTR_NAME: &str = "next";

impl Callback {
    pub fn from(f: &mut ImplItemFn) -> syn::Result<Option<Callback>> {
        let cmd = pop_attr(&mut f.attrs, COMMAND_ATTR_NAME)
            .map(FuncAttribute::parse)
            .transpose()?
            .map(Self::Command);
        let query = pop_attr(&mut f.attrs, QUERY_ATTR_NAME)
            .map(FuncAttribute::parse)
            .transpose()?
            .map(Self::Query);
        let action = pop_attr(&mut f.attrs, ACTION_ATTR_NAME)
            .map(FuncAttribute::parse)
            .transpose()?
            .map(Self::Action);

        let mut attrs = vec![cmd, query, action].into_iter().flatten();

        match (attrs.next(), attrs.next()) {
            (None, None) => Ok(None),
            (Some(c), None) => Ok(Some(c)),
            (_, Some(last)) => Err({
                syn::Error::new_spanned(last, format!("error: a function can only have one of: {ACTION_ATTR_NAME}, {QUERY_ATTR_NAME}, or {COMMAND_ATTR_NAME}"))
            }),
        }
    }
}

#[derive(Debug)]
struct Inherit {
    span: Span,
    expr: Expr,
}

impl Inherit {
    pub fn from(a: Attribute) -> syn::Result<Self> {
        Ok(Self {
            span: a.span(),
            expr: a.parse_args()?,
        })
    }

    pub fn to_tokens(&self, tform_ret: &impl Fn(&Span, TokenStream) -> TokenStream) -> TokenStream {
        (tform_ret)(&self.span, self.expr.to_token_stream())
    }

    pub fn to_cmds(&self) -> TokenStream {
        let e = &self.expr;

        quote_spanned! {
            self.span =>
            if let Some(r) = (#e).query(&mut query.clone().with_value(::serde_json::json!({"GetCommands":{}}))) {
                match r? {
                    ::chimera_ecs::prelude::Effect::Value(mut v, _) => {
                        cmds.append(v.as_array_mut().unwrap());
                    },
                    x => {
                        panic!("GetCommand response was not a value. Cannot inherit from this component. {:?}", x);
                    }
                }
            }
        }
    }
}

#[derive(Default, Debug)]
struct Inheritance {
    pub inherits: Vec<Inherit>,
}

impl Inheritance {
    pub fn pop_inheritance(i: &mut ItemImpl) -> syn::Result<Self> {
        let mut inherits = Vec::new();

        while let Some(a) = pop_attr(&mut i.attrs, "inherit") {
            inherits.push(Inherit::from(a)?);
        }

        Ok(Self { inherits })
    }

    pub fn to_stmt(&self, tform_ret: impl Fn(&Span, TokenStream) -> TokenStream) -> syn::Stmt {
        let i = self
            .inherits
            .iter()
            .map(|i| i.to_tokens(&tform_ret))
            .join_tokens_no_separation();

        parse_quote!(
            {
                #i
            }
        )
    }

    pub fn to_cmds(&self) -> syn::Stmt {
        let i = self
            .inherits
            .iter()
            .map(|i| i.to_cmds())
            .join_tokens_no_separation();

        parse_quote!(
            {
                #i
            }
        )
    }
}

#[derive(Debug)]
struct EntityAttrConfig {
    pub attr: Attribute,
    pub action_message_enum_name: Option<String>,
    pub query_message_enum_name: Option<String>,
    pub singleton: bool,
}

impl Parse for EntityAttrConfig {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attr = input.call(Attribute::parse_inner)?;

        Ok(Self::parse(attr.remove(0))?)
    }
}

impl EntityAttrConfig {
    pub fn parse(attr: Attribute) -> syn::Result<Self> {
        let mut config = Self {
            attr: attr.clone(),
            action_message_enum_name: None,
            query_message_enum_name: None,
            singleton: false,
        };

        let logic = |meta: ParseNestedMeta| {
            if meta.path.is_ident("action_message_enum_name") {
                config.action_message_enum_name = Some(meta.value()?.parse::<LitStr>()?.value());
                Ok(())
            } else if meta.path.is_ident("query_message_enum_name") {
                config.query_message_enum_name = Some(meta.value()?.parse::<LitStr>()?.value());
                Ok(())
            } else if meta.path.is_ident("singleton") {
                config.singleton = true;
                Ok(())
            } else {
                Err(meta.error("unexpected attribute: expected one of singleton, action_message_enum_name, query_message_enum_name"))
            }
        };

        attr.parse_nested_meta(logic)?;

        Ok(config)
    }
}

impl ToTokens for EntityAttrConfig {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.attr.to_tokens(tokens);
    }
}

#[derive(Debug)]
struct ComponentAttrConfig {
    pub attr: Attribute,
    pub action_message_enum_name: Option<String>,
    pub query_message_enum_name: Option<String>,
}

impl Parse for ComponentAttrConfig {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attr = input.call(Attribute::parse_inner)?;

        Ok(Self::parse(attr.remove(0))?)
    }
}

impl ComponentAttrConfig {
    pub fn parse(attr: Attribute) -> syn::Result<Self> {
        let mut config = Self {
            attr: attr.clone(),
            action_message_enum_name: None,
            query_message_enum_name: None,
        };

        let logic = |meta: ParseNestedMeta| {
            if meta.path.is_ident("action_message_enum_name") {
                config.action_message_enum_name = Some(meta.value()?.parse::<LitStr>()?.value());
                Ok(())
            } else if meta.path.is_ident("query_message_enum_name") {
                config.query_message_enum_name = Some(meta.value()?.parse::<LitStr>()?.value());
                Ok(())
            } else {
                Err(meta.error("unexpected attribute: expected one of action_message_enum_name, query_message_enum_name"))
            }
        };

        attr.parse_nested_meta(logic)?;

        Ok(config)
    }
}

impl ToTokens for ComponentAttrConfig {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.attr.to_tokens(tokens);
    }
}

#[derive(Debug)]
enum FuncParam {
    Field(Type),
    Message(Attribute),
    Next(Attribute),
    From(Attribute),
}

#[derive(Debug)]
struct FuncInfo {
    msg_name: Ident,
    fn_name: Ident,
    attr: FuncAttribute,
    docs: Vec<Attribute>,
    params: Vec<(Ident, FuncParam)>,
    debug_only: bool,
    ret: ReturnType,
}

#[derive(Debug, PartialEq)]
enum FuncFlag {
    Dispatchable,
    NoSerialize,
}

#[derive(Debug)]
struct FuncAttribute {
    pub attr: Attribute,
    pub message_name: Option<String>,
    pub flags: Vec<FuncFlag>,
}

impl FuncAttribute {
    pub fn parse(attr: Attribute) -> syn::Result<Self> {
        let mut config = Self {
            attr: attr.clone(),
            message_name: None,
            flags: Vec::new(),
        };

        let logic = |meta: ParseNestedMeta| {
            if meta.path.is_ident("message_name") {
                config.message_name = Some(meta.value()?.parse::<LitStr>()?.value());
                Ok(())
            } else if meta.path.is_ident("dispatchable") {
                config.flags.push(FuncFlag::Dispatchable);
                Ok(())
            } else if meta.path.is_ident("no_serialize") {
                config.flags.push(FuncFlag::Dispatchable);
                Ok(())
            } else {
                Err(meta.error("unexpected attribute: expected one of: dispatchable, message_name, no_serialize"))
            }
        };

        attr.parse_nested_meta(logic)?;

        Ok(config)
    }

    pub fn get_message_name(&self, sig: &syn::Signature) -> Ident {
        let name = match &self.message_name {
            Some(name) => name.clone(),
            None => sig
                .ident
                .to_string()
                .trim_start_matches("r#")
                .to_case(convert_case::Case::Pascal),
        };

        Ident::new(&name, sig.ident.span())
    }

    #[allow(dead_code)]
    pub fn has_flag(&self, flag: &FuncFlag) -> bool {
        self.flags.contains(flag)
    }
}

impl ToTokens for FuncAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.attr.to_tokens(tokens);
    }
}

macro_rules! optional {
    ($env:expr => $($t:tt)+) => {
        if matches!(std::env::var($env), Ok(x) if x == "true") {
            quote!()
        } else {
            quote!($($t)*)
        }
    };
}

fn get_context_type(entity: &ItemImpl) -> syn::Result<Type> {
    entity
        .trait_
        .as_ref()
        .ok_or_else(|| syn::Error::new(call_site(), "impl must be trait implementation"))?
        .pipe(|(_, t, _)| t.segments.last())
        .map(|l| match &l.arguments {
            syn::PathArguments::None => Ok(None),
            syn::PathArguments::AngleBracketed(a) => Ok(a.args.first()),
            other => Err(syn::Error::new(other.span(), "expected generics")),
        })
        .transpose()?
        .flatten()
        .map(|g| match g {
            syn::GenericArgument::Type(t) => Ok(t),
            other => Err(syn::Error::new(other.span(), "expected generic type")),
        })
        .transpose()?
        .cloned()
        .unwrap_or_else(|| parse_quote!(()))
        .pipe(Ok)
}

fn process_entity(
    config: EntityAttrConfig,
    mut entity: ItemImpl,
    mut funcs: ItemImpl,
) -> syn::Result<TokenStream> {
    let context_type = get_context_type(&entity)?;

    let (actions, queries) = detect_funcs(&mut entity)?;
    let inheritance = Inheritance::pop_inheritance(&mut entity)?;

    remove_funcs(&actions, &queries, &mut entity, &mut funcs);

    let (action_message_ident, action_tokens) = define_actions_enum(
        &config.action_message_enum_name,
        &entity.self_ty,
        &actions,
        config.singleton,
        &context_type,
    )?;
    let (query_message_ident, query_tokens) = define_queries_enum(
        &config.query_message_enum_name,
        &entity.self_ty,
        &queries,
        config.singleton,
        &context_type,
    )?;

    let mut m = generate_action_fn(&action_message_ident, &actions, &context_type)?;

    m.block.stmts.insert(
        0,
        inheritance.to_stmt(|s, e| {
            quote_spanned! {*s=>
                if let Some(r) = #e.handle(msg) {
                    return r;
                }
            }
        }),
    );

    entity.items.push(ImplItem::Fn(m));

    let mut m = generate_query_fn(
        &query_message_ident,
        &actions,
        &queries,
        &inheritance,
        &context_type,
    )?;

    m.block.stmts.insert(
        0,
        inheritance.to_stmt(|s, e| {
            quote_spanned! {*s=>
                if !query.is_message_type(&["GetCommands"]) && let Some(r) = #e.query(query) {
                    return r;
                }
            }
        }),
    );

    entity.items.push(ImplItem::Fn(m));

    let mut toks = proc_macro2::TokenStream::new();
    action_tokens.to_tokens(&mut toks);
    query_tokens.to_tokens(&mut toks);
    funcs.to_tokens(&mut toks);
    entity.to_tokens(&mut toks);

    Ok(toks.into_token_stream())
}

fn process_component(
    config: ComponentAttrConfig,
    mut component: ItemImpl,
) -> syn::Result<TokenStream> {
    let context_type = get_context_type(&component)?;

    let (actions, queries) = detect_funcs(&mut component)?;
    let inheritance = Inheritance::pop_inheritance(&mut component)?;

    let (action_message_ident, action_tokens) = define_actions_enum(
        &config.action_message_enum_name,
        &component.self_ty,
        &actions,
        false,
        &context_type,
    )?;
    let (query_message_ident, query_tokens) = define_queries_enum(
        &config.query_message_enum_name,
        &component.self_ty,
        &queries,
        false,
        &context_type,
    )?;

    emit_component_handle(
        action_message_ident,
        &actions,
        &mut component,
        &inheritance,
        &context_type,
    )?;
    emit_component_query(
        query_message_ident,
        actions,
        queries,
        &mut component,
        &inheritance,
        &context_type,
    )?;

    let mut toks = proc_macro2::TokenStream::new();
    action_tokens.to_tokens(&mut toks);
    query_tokens.to_tokens(&mut toks);
    component.to_tokens(&mut toks);

    Ok(toks.into_token_stream())
}

fn emit_component_handle(
    action_message_ident: Ident,
    actions: &Vec<FuncInfo>,
    component: &mut ItemImpl,
    inheritance: &Inheritance,
    context_type: &Type,
) -> syn::Result<()> {
    let mut handle = generate_action_fn(&action_message_ident, actions, context_type)?;

    handle.attrs.push(parse_quote!(#[doc(hidden)]));
    handle.sig.ident = format_ident!("__handle");
    component.items.push(ImplItem::Fn(handle));

    let mut pat = syn::PatOr {
        attrs: Vec::new(),
        leading_vert: None,
        cases: Punctuated::new(),
    };

    for q in actions {
        let i = &q.msg_name;
        let i: syn::Pat = parse_quote!(stringify!(#i));

        pat.cases.push(i);
    }

    let i = inheritance.to_stmt(|s, e| {
        quote_spanned! {*s=>
            if let Some(r) = #e.handle(msg) {
                return Some(r);
            }
        }
    });

    component.items.push(ImplItem::Fn(parse_quote! {
        pub fn handle(&mut self, msg: &mut ::chimera_ecs::prelude::Message) -> Option<::chimera_ecs::prelude::HandleResult> {
            #i
            if let Some(s) = msg.message_type() && matches!(s.as_str(), #pat) {
                Some(self.__handle(msg))
            } else {
                None
            }
        }
    }));

    Ok(())
}

fn emit_component_query(
    query_message_ident: Ident,
    actions: Vec<FuncInfo>,
    queries: Vec<FuncInfo>,
    component: &mut ItemImpl,
    inheritance: &Inheritance,
    context_type: &Type,
) -> syn::Result<()> {
    let mut query = generate_query_fn(
        &query_message_ident,
        &actions,
        &queries,
        inheritance,
        context_type,
    )?;

    query.attrs.push(parse_quote!(#[doc(hidden)]));
    query.sig.ident = format_ident!("__query");
    component.items.push(ImplItem::Fn(query));

    let mut pat = syn::PatOr {
        attrs: Vec::new(),
        leading_vert: None,
        cases: Punctuated::new(),
    };

    pat.cases.push(parse_quote!("GetCommands"));

    for q in &queries {
        let i = &q.msg_name;
        let i: syn::Pat = parse_quote!(stringify!(#i));

        pat.cases.push(i);
    }

    let i = inheritance.to_stmt(|s, e| {
        quote_spanned! {*s=>
            if !query.is_message_type(&["GetCommands"]) && let Some(r) = #e.query(query) {
                return Some(r);
            }
        }
    });

    component.items.push(ImplItem::Fn(parse_quote! {
        pub fn query(&self, query: &mut ::chimera_ecs::prelude::Query) -> Option<::chimera_ecs::prelude::QueryResult> {
            #i
            if let Some(s) = query.message_type() && matches!(s.as_str(), #pat) {
                Some(self.__query(query))
            } else {
                None
            }
        }
    }));

    Ok(())
}

fn detect_funcs(entity: &mut ItemImpl) -> syn::Result<(Vec<FuncInfo>, Vec<FuncInfo>)> {
    let mut actions = Vec::new();
    let mut queries = Vec::new();

    for f in entity.items.iter_mut() {
        if let ImplItem::Fn(m) = f {
            if let Some(c) = Callback::from(m)? {
                match c {
                    Callback::Command(attr) => {
                        m.attrs.push(parse_quote_spanned! {
                            attr.span() => #[cfg(debug_assertions)]
                        });

                        actions.push(FuncInfo {
                            fn_name: m.sig.ident.clone(),
                            msg_name: attr.get_message_name(&m.sig),
                            attr,
                            docs: m
                                .attrs
                                .iter()
                                .filter(|a| {
                                    a.path().get_ident().map(|i| i.to_string())
                                        == Some("doc".to_owned())
                                })
                                .cloned()
                                .collect(),
                            params: get_params(m, false)?,
                            debug_only: true,
                            ret: m.sig.output.clone(),
                        });
                    }
                    Callback::Action(attr) => {
                        actions.push(FuncInfo {
                            fn_name: m.sig.ident.clone(),
                            msg_name: attr.get_message_name(&m.sig),
                            attr,
                            docs: m
                                .attrs
                                .iter()
                                .filter(|a| {
                                    a.path().get_ident().map(|i| i.to_string())
                                        == Some("doc".to_owned())
                                })
                                .cloned()
                                .collect(),
                            params: get_params(m, false)?,
                            debug_only: false,
                            ret: m.sig.output.clone(),
                        });
                    }
                    Callback::Query(attr) => {
                        queries.push(FuncInfo {
                            fn_name: m.sig.ident.clone(),
                            msg_name: attr.get_message_name(&m.sig),
                            attr,
                            docs: m
                                .attrs
                                .iter()
                                .filter(|a| {
                                    a.path().get_ident().map(|i| i.to_string())
                                        == Some("doc".to_owned())
                                })
                                .cloned()
                                .collect(),
                            params: get_params(m, true)?,
                            debug_only: false,
                            ret: m.sig.output.clone(),
                        });
                    }
                }
            }
        }
    }

    Ok((actions, queries))
}

fn get_params(m: &mut ImplItemFn, is_query: bool) -> syn::Result<Vec<(Ident, FuncParam)>> {
    let items = m.sig.inputs.iter_mut()
        .map(|i| match i {
            syn::FnArg::Typed(syn::PatType { attrs, box pat, ty, .. }) => match pat {
                syn::Pat::Ident(i) => {

                    let message = pop_attr(attrs, if !is_query {
                        MESSAGE_ATTR_NAME
                    } else {
                        QUERY_ATTR_NAME
                    });
                    let next = if !is_query {
                        pop_attr(attrs, NEXT_ATTR_NAME)
                    } else {
                        None
                    };
                    let from = pop_attr(attrs, FROM_ATTR_NAME);

                    match (message, next, from) {
                        (Some(a), None, None) => {
                            Ok(Some((i.ident.clone(), FuncParam::Message(a))))
                        },
                        (None, Some(a), None) => {
                            Ok(Some((i.ident.clone(), FuncParam::Next(a))))
                        },
                        (None, None, Some(a)) => {
                            Ok(Some((i.ident.clone(), FuncParam::From(a))))
                        },
                        (None, None, None) => {
                            Ok(Some((i.ident.clone(), FuncParam::Field(*ty.clone()))))
                        },
                        _ => Err(syn::Error::new_spanned(i, format!("#[{MESSAGE_ATTR_NAME}], #[{FROM_ATTR_NAME}], and #[{NEXT_ATTR_NAME}] are mutually exclusive.")))
                    }
                },
                _ => Err(syn::Error::new_spanned(pat, "Must be an identifier"))
            },
            _ => Ok(None)
        })
        .try_collect::<Vec<_>>()?.into_iter()
        .flatten()
        .collect::<Vec<_>>();

    Ok(items)
}

fn remove_funcs(
    actions: &[FuncInfo],
    queries: &[FuncInfo],
    entity: &mut ItemImpl,
    funcs: &mut ItemImpl,
) {
    for a in actions.iter().chain(queries.iter()) {
        let f = entity.items.remove(
            entity
                .items
                .iter()
                .position(|item| matches!(item, ImplItem::Fn(m) if m.sig.ident == a.fn_name))
                .unwrap(),
        );

        let mut f = match f {
            ImplItem::Fn(m) => m,
            _ => panic!(),
        };

        for i in f.sig.inputs.iter_mut() {
            match i {
                syn::FnArg::Receiver(_) => {}
                syn::FnArg::Typed(syn::PatType { attrs, .. }) => {
                    pop_attr(attrs, MESSAGE_ATTR_NAME);
                    pop_attr(attrs, NEXT_ATTR_NAME);
                }
            }
        }

        funcs.items.push(ImplItem::Fn(f));
    }
}

fn define_actions_enum(
    action_message_enum_name: &Option<String>,
    self_ty: &Type,
    actions: &[FuncInfo],
    singleton: bool,
    context_type: &Type,
) -> syn::Result<(Ident, TokenStream)> {
    let action_message_ident = match action_message_enum_name {
        Some(name) => parse_quote_spanned! {
            call_site() => #name
        },
        None => format_ident!(
            "{}Message",
            match self_ty {
                syn::Type::Path(p) => match p.path.segments.iter().last() {
                    Some(p) => p.ident.clone(),
                    None => {
                        return Err(syn::Error::new(self_ty.span(), "Expected path. Cannot determine entity name for action message name. Set action_message_enum_name in the entity attribute."));
                    }
                },
                _ => {
                    return Err(syn::Error::new(self_ty.span(), "Expected path. Cannot determine entity name for action message name. Set action_message_enum_name in the entity attribute."));
                }
            }
        ),
    };

    let attrs = if matches!(std::env::var("CHIMERA_NO_SERDE"), Ok(x) if x == "true") {
        quote!()
    } else {
        quote!(#[derive(serde::Serialize, serde::Deserialize)])
    };

    let mut action_message: syn::ItemEnum = parse_quote_spanned! {
        call_site() =>
        #attrs
        #[derive(Debug, Clone)]
        pub enum #action_message_ident {

        }
    };

    define_enum_contents(&mut action_message, actions);
    let message_impl = define_action_enum_factories(
        &action_message_ident,
        self_ty,
        singleton,
        actions,
        context_type,
    );

    let payload_impl = define_enum_payload(&action_message_ident, actions);

    let mut toks = TokenStream::new();
    action_message.to_tokens(&mut toks);
    message_impl.to_tokens(&mut toks);
    payload_impl.to_tokens(&mut toks);

    Ok((action_message_ident, toks))
}

fn define_queries_enum(
    query_message_enum_name: &Option<String>,
    self_ty: &Type,
    queries: &[FuncInfo],
    singleton: bool,
    context_type: &Type,
) -> syn::Result<(Ident, TokenStream)> {
    let query_message_ident = match query_message_enum_name {
        Some(name) => parse_quote_spanned! {
            call_site() => #name
        },
        None => format_ident!(
            "{}Query",
            match self_ty {
                syn::Type::Path(p) => match p.path.segments.iter().last() {
                    Some(p) => p.ident.clone(),
                    None => {
                        return Err(syn::Error::new(self_ty.span(), "Expected path. Cannot determine entity name for query name. Set query_message_enum_name in the entity attribute."));
                    }
                },
                _ => {
                    return Err(syn::Error::new(self_ty.span(), "Expected path. Cannot determine entity name for query name. Set query_message_enum_name in the entity attribute."));
                }
            }
        ),
    };

    let attrs = if matches!(std::env::var("CHIMERA_NO_SERDE"), Ok(x) if x == "true") {
        quote!()
    } else {
        quote!(#[derive(serde::Serialize, serde::Deserialize)])
    };

    let mut query_message: syn::ItemEnum = parse_quote_spanned! {
        call_site() =>
        #attrs
        #[derive(Debug, Clone)]
        pub enum #query_message_ident {
            #[cfg(debug_assertions)]
            GetCommands{ }
        }
    };

    define_enum_contents(&mut query_message, queries);

    let mut entity_id: Ident = parse_quote!(entity_id);
    let i = 0;

    while queries
        .iter()
        .any(|a| a.params.iter().any(|(p, _)| p == &entity_id))
    {
        entity_id = format_ident!("entity_id{i}");
    }

    let message_impl = define_query_enum_factories(
        &query_message_ident,
        self_ty,
        singleton,
        queries,
        context_type,
    );

    let payload_impl = define_enum_payload(&query_message_ident, queries);

    let mut toks = TokenStream::new();
    query_message.to_tokens(&mut toks);
    message_impl?.to_tokens(&mut toks);
    payload_impl.to_tokens(&mut toks);

    Ok((query_message_ident, toks))
}

fn define_enum_contents(e: &mut syn::ItemEnum, fns: &[FuncInfo]) {
    for x in fns {
        let FuncInfo {
            ref msg_name,
            ref docs,
            ref attr,
            ..
        } = &x;

        let docs = docs.iter().join_tokens_no_separation();

        let debug: Option<TokenStream> = if x.debug_only {
            Some(quote_spanned! {attr.span() =>
                #[cfg(debug_assertions)]
            })
        } else {
            None
        };

        let mut serde = Vec::<TokenStream>::new();

        if attr.has_flag(&FuncFlag::NoSerialize) {
            serde.push(quote_spanned! {attr.span()=>
                skip
            });
        }

        let serde: Option<TokenStream> = if serde.len() > 0 {
            Some(quote_spanned! {attr.span()=>
                #[serde(#(#serde),*)]
            })
        } else {
            None
        };

        let mut item: syn::Variant = parse_quote_spanned! {
            attr.span() =>
            #debug
            #serde
            #docs
            #msg_name{}
        };

        match &mut item.fields {
            syn::Fields::Named(fields) => {
                for (pi, pt) in &x.params {
                    if let FuncParam::Field(ft) = &pt {
                        fields.named.push(syn::Field {
                            attrs: Vec::new(),
                            vis: syn::Visibility::Inherited,
                            ident: Some(pi.clone()),
                            colon_token: None,
                            ty: ft.clone(),
                            mutability: FieldMutability::None,
                        });
                    }
                }
            }
            _ => panic!(),
        }

        e.variants.push(item);
    }
}

fn define_action_enum_factories(
    message_ident: &Ident,
    self_ty: &Type,
    singleton: bool,
    fns: &[FuncInfo],
    context_type: &Type,
) -> syn::ItemImpl {
    let mut message_impl: syn::ItemImpl = parse_quote_spanned! {
        call_site() =>
        impl #message_ident {

        }
    };

    for action in fns {
        let FuncInfo {
            fn_name,
            params,
            debug_only,
            docs,
            msg_name,
            ..
        } = action;

        let docs = docs.iter().join_tokens_no_separation();

        let debug_attr: Option<Attribute> =
            debug_only.then(|| parse_quote!(#[cfg(debug_assertions)]));

        let mut f: syn::ItemFn = parse_quote_spanned! {fn_name.span()=>
            #debug_attr
            #docs
            fn #fn_name() -> ::chimera_ecs::prelude::Effect<#context_type> {

            }
        };

        let mut entity_id: Ident = parse_quote!(entity_id);
        let i = 1;

        while params.iter().any(|(p, _)| p == &entity_id) {
            entity_id = format_ident!("entity_id{i}");
        }

        if singleton {
            f.block.stmts.push(parse_quote_spanned! {fn_name.span()=>
                let #entity_id = #self_ty ::get_singleton_id();
            });
        } else {
            f.sig.inputs.push(parse_quote_spanned! {fn_name.span()=>
                #entity_id : ::chimera_ecs::prelude::Id
            });
        }

        let mut msg_fields = Vec::<&Ident>::new();

        for (pname, param) in params {
            if let FuncParam::Field(ptype) = param {
                f.sig.inputs.push(parse_quote_spanned! {pname.span()=>
                    #pname : #ptype
                });

                msg_fields.push(pname);
            }
        }

        let fields = msg_fields.iter().join_tokens::<Token![,]>();

        f.block.stmts.push(parse_quote_spanned!(fn_name.span()=>
            return ::chimera_ecs::prelude::effects::send(::chimera_ecs::prelude::Message::to(#entity_id).with_remote(Self::#msg_name{#fields}));
        ));

        message_impl.items.push(ImplItem::Fn(ImplItemFn {
            attrs: f.attrs,
            vis: syn::Visibility::Public(Default::default()),
            defaultness: None,
            sig: f.sig,
            block: *f.block,
        }));
    }

    message_impl
}

fn define_query_enum_factories(
    message_ident: &Ident,
    self_ty: &Type,
    singleton: bool,
    fns: &[FuncInfo],
    context_type: &Type,
) -> syn::Result<syn::ItemImpl> {
    let mut message_impl: syn::ItemImpl = parse_quote_spanned! {
        call_site() =>
        impl #message_ident {

        }
    };

    for query in fns {
        let FuncInfo {
            fn_name,
            params,
            debug_only,
            docs,
            msg_name,
            ret,
            ..
        } = query;

        let docs = docs.iter().join_tokens_no_separation();

        let debug_attr: Option<Attribute> =
            debug_only.then(|| parse_quote!(#[cfg(debug_assertions)]));

        let ret = match ret {
            ReturnType::Default => Box::new(parse_quote!(())),
            ReturnType::Type(_, t) => t.clone(),
        };

        let mut f: syn::ItemFn = parse_quote_spanned! {fn_name.span()=>
            #debug_attr
            #docs
            fn #fn_name() -> ::chimera_ecs::prelude::Effect<#context_type, <#ret as ::chimera_ecs::prelude::EffectValue>::Value> {

            }
        };

        let mut entity_id: Ident = parse_quote!(entity_id);
        let i = 1;

        while params.iter().any(|(p, _)| p == &entity_id) {
            entity_id = format_ident!("entity_id{i}");
        }

        if singleton {
            f.block.stmts.push(parse_quote_spanned! {fn_name.span()=>
                let #entity_id = #self_ty ::get_singleton_id();
            });
        } else {
            f.sig.inputs.push(parse_quote_spanned! {fn_name.span()=>
                #entity_id : ::chimera_ecs::prelude::Id
            });
        }

        let mut msg_fields = Vec::<&Ident>::new();

        for (pname, param) in params {
            if let FuncParam::Field(ptype) = param {
                f.sig.inputs.push(parse_quote_spanned! {pname.span()=>
                    #pname : #ptype
                });

                msg_fields.push(pname);
            }
        }

        let fields = msg_fields.iter().join_tokens::<Token![,]>();

        f.block.stmts.push(parse_quote_spanned!(fn_name.span() =>
            return ::chimera_ecs::prelude::effects::query_typed(
                ::chimera_ecs::prelude::Query::new(#entity_id).with_remote(Self::#msg_name{#fields})
            );
        ));

        message_impl.items.push(ImplItem::Fn(ImplItemFn {
            attrs: f.attrs,
            vis: syn::Visibility::Public(Default::default()),
            defaultness: None,
            sig: f.sig,
            block: *f.block,
        }));
    }

    Ok(message_impl)
}

fn define_enum_payload(message_ident: &Ident, fns: &[FuncInfo]) -> syn::ItemImpl {
    let message_types = fns
        .iter()
        .map(|f| {
            let label = &f.msg_name;
            parse_quote! {
                Self::#label { .. } => Some(stringify!(#label).into()),
            }
        })
        .collect::<Vec<syn::Arm>>();

    let no_serialize = fns
        .iter()
        .filter(|f| f.attr.has_flag(&FuncFlag::NoSerialize))
        .map(|f| {
            let label = &f.msg_name;
            parse_quote! {
                Self::#label { .. } => false,
            }
        })
        .collect::<Vec<syn::Arm>>();

    parse_quote! {
        impl chimera_core::prelude::RemotePayload for #message_ident {
            fn is_serializable(&self) -> bool {
                match self {
                    #(#no_serialize)*
                    _ => true
                }
            }

            fn serialize(&self) -> BoxedResult<Value> {
                Ok(serde_json::to_value(self)?)
            }

            fn get_message_type(&self) -> Option<std::borrow::Cow<'static, str>> {
                match self {
                    #(#message_types)*
                    _ => None
                }
            }
        }
    }
}

fn generate_fn_call(f: &FuncInfo, message_ident: Ident, body: Expr) -> Arm {
    Arm {
        attrs: Vec::new(),
        pat: syn::Pat::Struct(syn::PatStruct {
            attrs: Vec::new(),
            qself: None,
            path: {
                let mut p = syn::Path::from(message_ident);
                p.segments.push(syn::PathSegment::from(f.msg_name.clone()));
                p
            },
            brace_token: Brace::default(),
            fields: f
                .params
                .iter()
                .filter_map(|(pi, pt)| match pt {
                    FuncParam::Field(_) => Some(syn::FieldPat {
                        attrs: Vec::new(),
                        member: syn::Member::Named(pi.clone()),
                        colon_token: parse_quote!(:),
                        pat: {
                            let pi_out = format_ident!("param_{pi}");
                            parse_quote!(#pi_out)
                        },
                    }),
                    _ => None,
                })
                .collect(),
            rest: None,
        }),
        guard: None,
        fat_arrow_token: FatArrow::default(),
        body: Box::new(body),
        comma: None,
    }
}

fn generate_handle_arm(f: &FuncInfo, action_message_ident: &Ident) -> syn::Result<Arm> {
    let FuncInfo { ref fn_name, .. } = &f;

    let mut call: ExprMethodCall = parse_quote_spanned! {f.fn_name.span() =>
        self.#fn_name()
    };

    call.args = f
        .params
        .iter()
        .map(|(pi, pt)| {
            let x: Expr = match &pt {
                FuncParam::Field(_) => {
                    let pi = format_ident!("param_{pi}");
                    parse_quote!(#pi)
                }
                FuncParam::Message(a) => {
                    parse_quote_spanned!(a.span() => &msg)
                }
                FuncParam::Next(a) => {
                    parse_quote_spanned!(a.span() => msg.next.clone())
                }
                FuncParam::From(a) => {
                    parse_quote_spanned!(a.span() => msg.sender.clone())
                }
            };
            x
        })
        .collect();

    let mut arm = generate_fn_call(
        f,
        action_message_ident.clone(),
        if f.ret == ReturnType::Default || f.ret == parse_quote!(-> ()) {
            parse_quote_spanned! {f.fn_name.span()=>
                {
                    #call;
                    ::chimera_ecs::prelude::Effect::none()
                }
            }
        } else {
            call.into()
        },
    );

    if f.debug_only {
        arm.attrs = vec![parse_quote_spanned! { f.fn_name.span() => #[cfg(debug_assertions)] }];
    }

    Ok(arm)
}

fn generate_query_arm(f: &FuncInfo, query_message_ident: &Ident) -> syn::Result<Arm> {
    let FuncInfo { ref fn_name, .. } = &f;

    let mut call: ExprMethodCall = parse_quote_spanned! {f.attr.span() =>
        self.#fn_name()
    };

    call.args = f
        .params
        .iter()
        .map(|(pi, pt)| -> syn::Result<Expr> {
            match &pt {
                FuncParam::Field(_) => {
                    let pi = format_ident!("param_{pi}");
                    Ok(parse_quote!(#pi))
                }
                FuncParam::Message(a) => Ok(parse_quote_spanned!(a.span() => query)),
                FuncParam::Next(_) => {
                    panic!()
                }
                FuncParam::From(a) => Ok(parse_quote_spanned!(a.span() => query.sender.clone())),
            }
        })
        .try_collect()?;

    let call: Expr = if f.attr.has_flag(&FuncFlag::NoSerialize) {
        parse_quote_spanned! {f.attr.span() =>
            {
                let res = #call;
                ::chimera_ecs::prelude::Effect::deferred(move |e| {
                    Ok(::chimera_core::prelude::Response::Local(Box::new(res.evaluate(e)?)))
                })
            }
        }
    } else {
        parse_quote_spanned! {f.attr.span() =>
            {
                let res = #call;
                ::chimera_ecs::prelude::Effect::deferred(move |e| {
                    Ok(::chimera_core::prelude::Response::Remote(Box::new(res.evaluate(e)?)))
                })
            }
        }
    };

    let mut arm = generate_fn_call(f, query_message_ident.clone(), call);

    if f.debug_only {
        arm.attrs = vec![parse_quote_spanned! { f.attr.span() => #[cfg(debug_assertions)] }];
    }

    Ok(arm)
}

#[derive(serde::Serialize)]
enum CommandParameterType {
    Int,
    Float,
    String,
    Boolean,
    Unknown,
}

#[derive(serde::Serialize)]
struct CommandParameter {
    param_type: CommandParameterType,
    optional: bool,
}

#[derive(serde::Serialize)]
struct CommandDefinition {
    command: String,
    params: HashMap<String, CommandParameter>,
}

fn generate_command_response(actions: &[FuncInfo], inheritance: &Inheritance) -> Expr {
    let cmds = actions
        .iter()
        .filter(|f| f.debug_only)
        .map(get_command_definition)
        .collect::<Vec<_>>();

    let cmds = serde_json::to_string(&cmds).unwrap();

    let inherited_cmds = inheritance.to_cmds();

    parse_quote_spanned! {
        call_site() =>
        {
            let mut cmds = Vec::new();
            cmds.append(serde_json::from_str::<serde_json::Value>(#cmds).map_err(|e| Box::new(e) as utils::BoxedError)?.as_array_mut().unwrap());
            #inherited_cmds
            ::chimera_ecs::prelude::Effect::value(::chimera_core::prelude::Response::Value(serde_json::Value::Array(cmds)))
        }
    }
}

fn get_command_definition(f: &FuncInfo) -> CommandDefinition {
    CommandDefinition {
        command: f.msg_name.to_string(),
        params: f
            .params
            .iter()
            .filter_map(|(pi, pt)| {
                let mut pt = match &pt {
                    FuncParam::Field(pt) => pt.clone(),
                    _ => {
                        return None;
                    }
                };
                let mut pt2 = None;

                let optional = match &pt {
                    Type::Path(p) => {
                        let i = p.path.segments.last().unwrap();

                        if i.ident == format_ident!("Option")
                            && let syn::PathArguments::AngleBracketed(a) = &i.arguments
                        {
                            pt2 = match a.args.iter().next().unwrap() {
                                syn::GenericArgument::Type(t) => Some(t.clone()),
                                _ => panic!(),
                            };
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                };

                if let Some(p) = pt2 {
                    pt = p;
                }

                Some((
                    pi.to_string(),
                    CommandParameter {
                        param_type: match pt {
                            Type::Path(p) if matches!(p.path.get_ident(), Some(_)) => {
                                let t = p.path.to_token_stream().to_string();
                                match t.as_str() {
                                    "i32" | "i64" | "u32" | "u64" => CommandParameterType::Int,
                                    "f32" | "f64" => CommandParameterType::Float,
                                    "bool" => CommandParameterType::Boolean,
                                    "String" | "&str" => CommandParameterType::String,
                                    _ => CommandParameterType::Unknown,
                                }
                            }
                            _ => CommandParameterType::Unknown,
                        },
                        optional,
                    },
                ))
            })
            .collect::<HashMap<_, _>>(),
    }
}

fn generate_action_enum_items(
    action_message_ident: &Ident,
    actions: &[FuncInfo],
) -> syn::Result<Vec<syn::Arm>> {
    Ok(actions
        .iter()
        .map(|f| generate_handle_arm(f, action_message_ident))
        .try_collect()?)
}

fn generate_query_enum_items(
    query_message_ident: &Ident,
    actions: &[FuncInfo],
    queries: &[FuncInfo],
    inheritance: &Inheritance,
) -> syn::Result<Vec<syn::Arm>> {
    let cmds = generate_command_response(actions, inheritance);

    let x: syn::ExprMatch = parse_quote_spanned! {
        call_site() =>
        match todo!() {
            #[cfg(debug_assertions)]
            #query_message_ident::GetCommands{ } => #cmds
        }
    };

    let mut arms = x.arms;

    arms.append(
        &mut queries
            .iter()
            .map(|f| generate_query_arm(f, query_message_ident))
            .try_collect()?,
    );

    Ok(arms)
}

fn generate_action_fn(
    action_message_ident: &Ident,
    actions: &[FuncInfo],
    context_type: &Type,
) -> syn::Result<syn::ImplItemFn> {
    let trace = optional!("CHIMERA_NO_TRACING" => tracing::debug!(what = "Received Message", who = ?self, ?msg););
    let inspect = optional!("CHIMERA_NO_TRACING" =>
        .inspect_err(|e| tracing::error!(what = "Error deserializing Message", who = stringify!(#action_message_ident), why = %e))
    );

    let arms = generate_action_enum_items(action_message_ident, actions)?
        .into_iter()
        .join_tokens::<Token![,]>();

    Ok(parse_quote! {
        #[allow(unused_parens)]
        fn handle(
            &mut self,
            msg: &mut ::chimera_ecs::prelude::Message,
        ) -> ::chimera_ecs::prelude::Effect<#context_type> {
            #trace
            match msg.try_convert_first::<#action_message_ident>()#inspect? {
                #arms
            }
        }
    })
}

fn generate_query_fn(
    query_message_ident: &Ident,
    actions: &[FuncInfo],
    queries: &[FuncInfo],
    inheritance: &Inheritance,
    context_type: &Type,
) -> syn::Result<syn::ImplItemFn> {
    let trace = optional!("CHIMERA_NO_TRACING" => tracing::debug!(what = "Received Query", who = ?self, ?query););
    let inspect = optional!("CHIMERA_NO_TRACING" =>
        .inspect_err(|e| tracing::error!(what = "Error deserializing Query", who = stringify!(#query_message_ident), why = %e))
    );

    let arms = generate_query_enum_items(query_message_ident, actions, queries, inheritance)?
        .into_iter()
        .join_tokens::<Token![,]>();

    Ok(parse_quote! {
        fn query(
            &self,
            query: &mut ::chimera_ecs::prelude::Query,
        ) -> ::chimera_ecs::prelude::Effect<#context_type, ::chimera_core::prelude::Response> {
            #trace
            match query.try_convert::<#query_message_ident>()#inspect? {
                #arms
            }
        }
    })
}

#[proc_macro_attribute]
pub fn entity(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let config = parse_macro_input!(attr as EntityAttrConfig);
    let entity_impl = parse_macro_input!(input as ItemImpl);
    let mut funcs_impl: ItemImpl = parse_quote!(
        impl entity {}
    );

    funcs_impl.self_ty = entity_impl.self_ty.clone();
    funcs_impl.generics = entity_impl.generics.clone();

    match process_entity(config, entity_impl, funcs_impl) {
        Ok(s) => s.into(),
        Err(s) => s.to_compile_error().into(),
    }
}

#[proc_macro_attribute]
pub fn component(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let config = parse_macro_input!(attr as ComponentAttrConfig);
    let component_impl = parse_macro_input!(input as ItemImpl);

    match process_component(config, component_impl) {
        Ok(s) => s.into(),
        Err(s) => s.to_compile_error().into(),
    }
}
