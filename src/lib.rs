use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Expr, ExprArray, Ident, Token, braced,
    parse::{self, Parse, ParseStream},
    parse_macro_input,
    token::Brace,
};

#[proc_macro]
pub fn layout_macro(input: TokenStream) -> TokenStream {
    let layout = parse_macro_input!(input as Layout);

    let attrs = layout.attrs;
    let ident = layout.ident;
    let from = extract_nested_arrays(layout.arm.from).unwrap();
    let to = extract_nested_arrays(layout.arm.to).unwrap();

    let mut set = HashSet::new();

    let pattern = {
        let pattern = from.iter().map(|row| {
            let idents = row.iter().map(|expr| {
                // TODO: This sucks
                let ident = if let Expr::Path(path) = expr {
                    if let Some(ident) = path.path.get_ident() {
                        ident
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                };

                set.insert(ident.to_string());
                quote! { $#ident:expr }
            });
            quote! { [ #( #idents ),* $(,)? ] }
        });

        quote! { #( #pattern ),* $(,)? }
    };

    let expansion = {
        let pattern = to.iter().map(|row| {
            let exprs = row.iter().map(|expr| {
                // TODO: This sucks even more
                if let Expr::Path(path) = expr {
                    if let Some(ident) = path.path.get_ident() {
                        if set.contains(&ident.to_string()) {
                            return quote! { $#expr };
                        }
                    }
                }

                quote! { #expr }
            });

            quote! { [ #( #exprs ),* ] }
        });

        quote! { [ #( #pattern ),* ] }
    };

    let macro_ = quote! {
        #( #attrs )*
        macro_rules! #ident {
            ( #pattern ) => { #expansion };
        }
    };

    macro_.into()
}

struct Layout {
    attrs: Vec<Attribute>,
    _macro: Token![macro],
    ident: Ident,
    _brace: Brace,
    arm: Arm,
}

impl Parse for Layout {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Layout {
            attrs: input.call(Attribute::parse_outer)?,
            _macro: input.parse()?,
            ident: input.parse()?,
            _brace: braced!(content in input),
            arm: content.parse()?,
        })
    }
}

struct Arm {
    from: ExprArray,
    _fat_arrow: Token![=>],
    to: ExprArray,
}

impl Parse for Arm {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(Arm {
            from: input.parse()?,
            _fat_arrow: input.parse()?,
            to: input.parse()?,
        })
    }
}

fn extract_nested_arrays(array: ExprArray) -> Option<Vec<Vec<Expr>>> {
    array
        .elems
        .into_iter()
        .map(|x| match x {
            Expr::Array(a) => Some(a.elems.into_iter().collect()),
            _ => None,
        })
        .collect()
}
