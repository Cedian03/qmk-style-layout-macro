use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    Attribute, Expr, Ident, Token, braced, bracketed,
    parse::{self, Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token::{Brace, Bracket},
};

#[proc_macro]
pub fn layout_macro(input: TokenStream) -> TokenStream {
    let layout = parse_macro_input!(input as Layout);

    match layout_macro_impl(layout) {
        Ok(tokens) => tokens,
        Err(err) => err.into_compile_error().into(),
    }
}

fn layout_macro_impl(layout: Layout) -> syn::Result<TokenStream> {
    let attrs = layout.attrs;
    let macro_ident = layout.ident;

    let mut params = HashSet::new();

    let foo = layout
        .arm
        .pattern
        .items
        .into_iter()
        .map(|row| {
            row.items
                .into_iter()
                .map(|Param { ident, .. }| {
                    if params.insert(ident.to_string()) {
                        Ok(ident)
                    } else {
                        Err(syn::Error::new(ident.span(), "Parameter defined twice"))
                    }
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;

    let pattern = quote! { #( [#($#foo:expr),* $(,)? ]),* $(,)? };

    println!("{}", &pattern);

    let bar = layout
        .arm
        .expansion
        .items
        .into_iter()
        .map(|row| {
            row.items.into_iter().map(|item| match item {
                Item::Param(Param { ident, .. }) => {
                    if params.contains(&ident.to_string()) {
                        quote! { $#ident }
                    } else {
                        quote_spanned!(
                            ident.span() => compile_error!(concat!("Unidentified parameter: ", stringify!(#ident)))
                        )
                    }
                }
                Item::Expr(expr) => quote! { #expr },
            })
            .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let expansion = quote! { [#( [#(#bar),*] ),*] };

    println!("{}", &expansion);

    let macro_ = quote! {
        #( #attrs )*
        macro_rules! #macro_ident {
            ( #pattern ) => { #expansion };
        }
    };

    println!("{}", &macro_);

    Ok(macro_.into())
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
    pattern: Matrix<Param>,
    _fat_arrow: Token![=>],
    expansion: Matrix<Item>,
}

impl Parse for Arm {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(Arm {
            pattern: input.parse()?,
            _fat_arrow: input.parse()?,
            expansion: input.parse()?,
        })
    }
}

type Matrix<T> = Array<Array<T>>;

struct Array<T> {
    _bracket: Bracket,
    items: Punctuated<T, Token![,]>,
}

impl<T: Parse> Parse for Array<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Self {
            _bracket: bracketed!(content in input),
            items: content.parse_terminated(T::parse, Token![,])?,
        })
    }
}

enum Item {
    Param(Param),
    Expr(Expr),
}

impl Parse for Item {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(Token![#]) {
            Self::Param(input.parse()?)
        } else {
            Self::Expr(input.parse()?)
        })
    }
}

struct Param {
    _pound: Token![#],
    ident: Ident,
}

impl Parse for Param {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            _pound: input.parse()?,
            ident: input.parse()?,
        })
    }
}
