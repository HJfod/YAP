
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate darling;

use darling::ast::NestedMeta;
use darling::{ast, FromDeriveInput, FromField, FromMeta, FromVariant};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, Fields, Ident, ItemEnum, Meta, Path, Type};
use syn::spanned::Spanned;

// https://stackoverflow.com/questions/55271857/how-can-i-get-the-t-from-an-optiont-when-using-syn
fn extract_type_from_option(ty: &syn::Type) -> Option<&syn::Type> {
    use syn::{GenericArgument, PathArguments, PathSegment};

    fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
        match *ty {
            syn::Type::Path(ref typepath) if typepath.qself.is_none() => Some(&typepath.path),
            _ => None,
        }
    }

    // TODO store (with lazy static) the vec of string
    // TODO maybe optimization, reverse the order of segments
    fn extract_option_segment(path: &Path) -> Option<&PathSegment> {
        let idents_of_path = path
            .segments
            .iter()
            .fold(String::new(), |mut acc, v| {
                acc.push_str(&v.ident.to_string());
                acc.push('|');
                acc
            });
        vec!["Option|", "std|option|Option|", "core|option|Option|"]
            .into_iter()
            .find(|s| idents_of_path == *s)
            .and_then(|_| path.segments.last())
    }

    extract_type_path(ty)
        .and_then(|path| extract_option_segment(path))
        .and_then(|path_seg| {
            let type_params = &path_seg.arguments;
            // It should have only on angle-bracketed param ("<String>"):
            match *type_params {
                PathArguments::AngleBracketed(ref params) => params.args.first(),
                _ => None,
            }
        })
        .and_then(|generic_arg| match *generic_arg {
            GenericArgument::Type(ref ty) => Some(ty),
            _ => None,
        })
}

#[derive(FromDeriveInput)]
#[darling(attributes(parse), supports(any))]
struct ParseReceiver {
    ident: syn::Ident,
    generics: syn::Generics,
    data: ast::Data<ParseVariant, ParseField>,
    expected: Option<String>,
}

#[derive(FromVariant)]
struct ParseVariant {
    ident: syn::Ident,
    fields: ast::Fields<ParseField>,
}

#[derive(FromField)]
#[darling(attributes(parse))]
struct ParseField {
    ident: Option<syn::Ident>,
    ty: Type,
    #[darling(default)]
    skip: bool,
}

impl ToTokens for ParseReceiver {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match &self.data {
            ast::Data::Struct(data) => {
                if self.expected.is_some() {
                    tokens.extend(
                        syn::Error::new(
                            self.expected.span(),
                            "cannot use \"expected\" on a struct"
                        ).to_compile_error()
                    );
                }
            }
            ast::Data::Enum(data) => {
                let impl_for_name = &self.ident;
                tokens.extend(quote! {
                    impl<'s> prolangine::parse::node::NodeKind for #impl_for_name <'s> {
                        
                    }
                });
            }
        }
    }
}

#[proc_macro_derive(NodeKind, attributes(parse))]
pub fn derive_node_kind(input: TokenStream) -> TokenStream {
    match ParseReceiver::from_derive_input(&syn::parse(input).expect("Couldn't parse item")) {
        Ok(v) => v,
        Err(e) => {
            return e.write_errors().into();
        }
    }.to_token_stream().into()
}

#[derive(FromMeta)]
struct CreateTokenNodesArgs {}

#[proc_macro_attribute]
pub fn create_token_nodes(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => { return TokenStream::from(darling::Error::from(e).write_errors()); }
    };
    let args = match CreateTokenNodesArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => { return TokenStream::from(e.write_errors()); }
    };
    let input = parse_macro_input!(input as ItemEnum);
    let token_vis = &input.vis;
    let token_name = &input.ident;
    let mut display_names = TokenStream2::new();

    let mut result = quote! {
        #[derive(Debug)]
        #input
    };
    for variant in input.variants {
        if matches!(variant.fields, Fields::Named(_)) {
            result.extend(
                syn::Error::new(
                    variant.fields.span(),
                    "token variants must have unit or tuple type"
                ).to_compile_error()
            );
            continue;
        }

        let variant_name = &variant.ident;
        let node_name = format_ident!("{}Token", variant.ident);
        
        let mut destructure_names = quote! {};
        let mut fabricate = quote! {};
        let mut ty = quote! {};
        for (ix, field) in variant.fields.iter().enumerate() {
            let vty = &field.ty;
            let des = format_ident!("v_{ix}");
            ty.extend(quote! { #vty, });
            destructure_names.extend(quote! { #des, });
            fabricate.extend(quote! { prolangine::parse::node::NodeKind::fabricate(span.clone()), });
        }
        // Make the value type a tuple (or unit)
        ty = quote! { (#ty) };

        // On unit variants, we don't need to destructure in any way
        let destructure = if matches!(variant.fields, Fields::Unnamed(_)) {
            quote! { (#destructure_names) }
        }
        else {
            quote! {}
        };

        let mut display_name = variant_name.to_string().to_lowercase();

        for attr in variant.attrs {
            if attr.meta.path().get_ident().map(|i| i.to_string()) == Some(String::from("token")) {
                match attr.meta {
                    Meta::List(list) => {}
                    _ => {
                        result.extend(
                            syn::Error::new(
                                variant.fields.span(),
                                "expected arguments for token(...)"
                            ).to_compile_error()
                        );
                    }
                }
            }
        }

        display_names.extend(quote! {
            #token_name::#variant_name #destructure => #display_name,
        });

        result.extend(quote! {
            #[derive(Debug)]
            #token_vis struct #node_name {
                value: #ty,
                span: prolangine::src::Span,
            }

            impl prolangine::parse::node::Parse<#token_name> for #node_name {
                fn parse<I>(tokenizer: &mut I) -> prolangine::parse::node::Node<Self>
                    where
                        Self: Sized,
                        I: prolangine::parse::token::TokenIterator<#token_name>
                {
                    let token = tokenizer.next();
                    let span = token.span();
                    if let Some(#token_name::#variant_name #destructure) = token.as_token() {
                        let span = token.span();
                        let Some(#token_name::#variant_name #destructure) = token.into_token() else {
                            unreachable!();
                        };
                        prolangine::parse::node::Node::from(#node_name {
                            value: (#destructure_names),
                            span,
                        })
                    }
                    else {
                        prolangine::parse::node::Node::expected(#display_name, &token, span.clone())
                    }
                }
                fn peek<I>(tokenizer: &I) -> bool
                    where
                        Self: Sized,
                        I: prolangine::parse::token::TokenIterator<#token_name>
                {
                    matches!(tokenizer.peek().as_token(), Some(#token_name::#variant_name #destructure))
                }
            }

            impl prolangine::parse::node::NodeKind for #node_name {
                fn children(&self) -> Vec<&dyn NodeKind> {
                    vec![]
                }
                fn span(&self) -> Span {
                    self.span.clone()
                }
            }
        });
    }
    result.extend(quote! {
        impl prolangine::parse::token::DisplayName for #token_name {
            fn display_name(&self) -> String {
                String::from(match self {
                    #display_names
                })
            }
        }
    });
    result.into()
}
