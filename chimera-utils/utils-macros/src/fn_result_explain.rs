
use quote::ToTokens;
use syn::{parse::Parse, parse_quote_spanned, Token, spanned::Spanned};

pub enum ResultExplainParameter {
    Explain(syn::LitStr),
    ExplainWith(syn::ExprClosure)
}

impl Parse for ResultExplainParameter {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![|]) {
            Ok(Self::ExplainWith(input.parse()?))
        } else {
            Ok(Self::Explain(input.parse()?))
        }
    }
}

pub struct ResultExplainInput(syn::ItemFn);

impl Parse for ResultExplainInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self(input.parse()?))
    }
}

pub struct ResultExplainOutput(syn::ItemFn);

impl ToTokens for ResultExplainOutput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.0.to_tokens(tokens)
    }
}

impl ResultExplainParameter {
    pub fn transform(self, input: ResultExplainInput) -> ResultExplainOutput {

        let f = input.0;

        let mut out = f.clone();

        let contents = &f.block;

        let ret = &f.sig.output;

        out.block = match self {
            ResultExplainParameter::Explain(s) => {
                parse_quote_spanned! {s.span() =>
                    {
                        let res: #ret = (move || {#contents})();

                        Ok(::utils::MapExplainExt::map_err_explain(res, #s)?)
                    }
                }
            },
            ResultExplainParameter::ExplainWith(e) => {
                parse_quote_spanned! {e.span() =>
                    {
                        let res: #ret = (move || {#contents})();

                        Ok(::utils::MapExplainExt::map_err_explain_with(res, #e)?)
                    }
                }
            },
        };

        ResultExplainOutput(out)
    }
}
