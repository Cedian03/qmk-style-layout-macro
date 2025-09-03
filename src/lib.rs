use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Expr, ExprArray, Token,
    parse::{self, Parse, ParseStream},
    parse_macro_input,
};

struct LayoutInput {
    from: ExprArray,
    _as: Token![as],
    to: ExprArray,
}

impl Parse for LayoutInput {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let from: ExprArray = input.parse()?;
        let _as: Token![as] = input.parse()?;
        let to: ExprArray = input.parse()?;
        Ok(LayoutInput { from, _as, to })
    }
}

#[proc_macro]
pub fn create_layout_macro(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as LayoutInput);

    let from = extract_nested_arrays(input.from).unwrap();
    let to = extract_nested_arrays(input.to).unwrap();

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

        quote! { [ #( #pattern ),* $(,)? ] }
    };

    // println!("Pattern: {}", pattern);

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

    // println!("Expansion: {}", expansion);

    let macro_ = quote! {
        macro_rules! layout_macro {
            ( #pattern ) => { #expansion }
        }
    };

    // println!("Macro: {}", &macro_);

    macro_.into()
}

fn extract_nested_arrays(array: ExprArray) -> Option<Vec<Vec<Expr>>> {
    array
        .elems
        .into_iter()
        .map(|x| match x {
            Expr::Array(y) => Some(y.elems.into_iter().collect()),
            _ => None,
        })
        .collect()
}
