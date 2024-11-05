
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate darling;

use darling::{FromDeriveInput, ast, FromField, FromVariant};
use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Ident};
use quote::{quote, quote_spanned, ToTokens};
use syn::parse::Parse;
use syn::{Generics, Type, Path};
use syn::spanned::Spanned;
use syn::parse::Parser;

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
                    impl<'s> prolangine::parse::node::NodeKind<'s> for #impl_for_name <'s> {
                        
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
