use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{Attribute, Meta};

pub fn pop_attr(attrs: &mut Vec<Attribute>, name: &str) -> Option<Attribute> {
    let mut i = attrs.iter().enumerate();
    let attr = loop {
        match i.next() {
            Some((
                idx,
                Attribute {
                    meta: Meta::Path(path),
                    ..
                },
            )) => {
                if let Some(ident) = path.get_ident()
                    && *ident == name
                {
                    break Some(idx);
                }
            }
            Some(_) => {}
            None => break None,
        }
    };

    attr.map(|idx| attrs.remove(idx))
}

#[cfg(not(test))]
pub fn call_site() -> proc_macro2::Span {
    proc_macro::Span::call_site().into()
}

#[cfg(test)]
pub fn call_site() -> proc_macro2::Span {
    proc_macro2::Span::mixed_site()
}

pub trait JoinTokens {
    fn join_tokens_no_separation(self) -> TokenStream;
    fn join_tokens<T: ToTokens + Default>(self) -> TokenStream;
}

impl<I: Iterator<Item: ToTokens>> JoinTokens for I {
    fn join_tokens_no_separation(self) -> TokenStream {
        let mut t = TokenStream::new();

        for x in self {
            x.to_tokens(&mut t);
        }

        t
    }

    fn join_tokens<T2: ToTokens + Default>(mut self) -> TokenStream {
        let mut t = TokenStream::new();

        if let Some(x) = self.next() {
            x.to_tokens(&mut t);
        }

        for x in self {
            T2::default().to_tokens(&mut t);
            x.to_tokens(&mut t);
        }

        t
    }
}
