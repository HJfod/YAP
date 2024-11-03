
// extern crate proc_macro;
// extern crate proc_macro2;
// extern crate syn;
// extern crate quote;
// extern crate darling;

// use darling::{FromDeriveInput, ast, FromField, FromVariant};
// use darling::FromMeta;
// use proc_macro::TokenStream;
// use proc_macro2::{TokenStream as TokenStream2, Ident};
// use quote::{quote, quote_spanned, ToTokens};
// use syn::parse::Parse;
// use syn::{Generics, Type, Path};
// use syn::spanned::Spanned;
// use syn::parse::Parser;

// // https://stackoverflow.com/questions/55271857/how-can-i-get-the-t-from-an-optiont-when-using-syn
// fn extract_type_from_option(ty: &syn::Type) -> Option<&syn::Type> {
//     use syn::{GenericArgument, PathArguments, PathSegment};

//     fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
//         match *ty {
//             syn::Type::Path(ref typepath) if typepath.qself.is_none() => Some(&typepath.path),
//             _ => None,
//         }
//     }

//     // TODO store (with lazy static) the vec of string
//     // TODO maybe optimization, reverse the order of segments
//     fn extract_option_segment(path: &Path) -> Option<&PathSegment> {
//         let idents_of_path = path
//             .segments
//             .iter()
//             .fold(String::new(), |mut acc, v| {
//                 acc.push_str(&v.ident.to_string());
//                 acc.push('|');
//                 acc
//             });
//         vec!["Option|", "std|option|Option|", "core|option|Option|"]
//             .into_iter()
//             .find(|s| idents_of_path == *s)
//             .and_then(|_| path.segments.last())
//     }

//     extract_type_path(ty)
//         .and_then(|path| extract_option_segment(path))
//         .and_then(|path_seg| {
//             let type_params = &path_seg.arguments;
//             // It should have only on angle-bracketed param ("<String>"):
//             match *type_params {
//                 PathArguments::AngleBracketed(ref params) => params.args.first(),
//                 _ => None,
//             }
//         })
//         .and_then(|generic_arg| match *generic_arg {
//             GenericArgument::Type(ref ty) => Some(ty),
//             _ => None,
//         })
// }

// fn impl_ast_item(
//     target: &impl ToTokens, target_name: &Ident, target_generics: &Generics,
//     parse_impl: TokenStream2, peek_impl: TokenStream2, children_impl: TokenStream2,
//     span_impl: Option<TokenStream2>,
// ) -> TokenStream2 {
//     let (impl_generics, ty_generics, where_clause) = target_generics.split_for_impl();
//     quote! {
//         #target
//         impl #impl_generics crate::parse::NodeKind<'s> for #target_name #ty_generics #where_clause {
//             fn children(&self) -> Vec<&dyn crate::checker::resolve::ResolveRef> {
//                 #children_impl
//             }
//             #span_impl
//         }
//     }
// }

// #[derive(FromDeriveInput)]
// #[darling(attributes(parse), supports(any))]
// struct ParseReceiver {
//     ident: syn::Ident,
//     generics: syn::Generics,
//     data: ast::Data<ParseVariant, ParseField>,
//     expected: Option<String>,
// }

// #[derive(FromVariant)]
// struct ParseVariant {
//     ident: syn::Ident,
//     fields: ast::Fields<ParseField>,
// }

// #[derive(FromField)]
// #[darling(attributes(parse))]
// struct ParseField {
//     ident: Option<syn::Ident>,
//     ty: Type,
//     #[darling(default)]
//     skip: bool,
// }

// fn field_to_tokens(data: &ast::Fields<ParseField>, self_name: Path) -> (TokenStream2, TokenStream2, TokenStream2) {
//     let mut children_impl = quote! {};
//     let mut parse_impl = quote! {};
//     let mut peek_impl = quote! {};

//     // Automatically figure out peek point if it wasn't manually set
//     let mut peek_count = 0;
//     for field in data.iter() {
//         // Break unless the type is optional, in which case 
//         // continue peeking since an optional field is not 
//         // enough to determine exhaustively
//         if extract_type_from_option(&field.ty).is_none() {
//             peek_count += 1;
//             break;
//         }
//     }
    
//     // Generate parse and peek impls
//     let mut peek_ix = 0usize;
//     for (field_ix, field) in data.iter().enumerate() {
//         if field.skip {
//             if let Some(ref i) = field.ident {
//                 parse_impl.extend(quote! { #i: Default::default(), });
//             }
//             else {
//                 parse_impl.extend(quote! { Default::default(), });
//             }
//         }
//         else {
//             let t = &field.ty;
//             if let Some(ref i) = field.ident {
//                 parse_impl.extend(quote! {
//                     #i: crate::parse::NodeKind::parse(tokenizer, logger),
//                 });
//                 children_impl.extend(quote! {
//                     (&self.#i as &dyn crate::parse::NodeKind),
//                 });
//             }
//             else {
//                 parse_impl.extend(quote! {
//                     crate::parse::NodeKind::parse(tokenizer, logger),
//                 });
//                 children_impl.extend(quote! {
//                     (&self.#field_ix as &dyn crate::parse::NodeKind),
//                 });
//             }
//             if peek_ix < peek_count {
//                 peek_impl.extend(quote! {
//                     if <#t>::peek(#peek_ix, tokenizer) {
//                         peeked += 1;
//                     }
//                 });
//                 if extract_type_from_option(t).is_none() {
//                     peek_ix += 1;
//                 }
//             }
//         }
//     }
//     (
//         if data.is_struct() {
//             quote! {
//                 use crate::parser::parse::Node;
//                 use crate::shared::src::ArcSpan;
//                 let r = #self_name {
//                     #parse_impl
//                 };
//                 Ok(pool.add(r))
//             }
//         }
//         else if data.is_unit() {
//             quote! {
//                 Ok(pool.add(#self_name))
//             }
//         }
//         else {
//             quote! {
//                 use crate::parser::parse::Node;
//                 use crate::shared::src::ArcSpan;
//                 let r = #self_name(
//                     #parse_impl
//                 );
//                 Ok(pool.add(r))
//             }
//         },
//         quote! {
//             use crate::parser::parse::ParseRef;
//             #peek_checks
//             let mut peeked = 0;
//             #peek_impl
//             peeked == #peek_count
//         },
//         quote! {
//             vec![#children_impl]
//         }
//     )
// }

// impl ToTokens for ParseReceiver {
//     fn to_tokens(&self, tokens: &mut TokenStream2) {
//         match &self.data {
//             ast::Data::Struct(data) => {
//                 if self.expected.is_some() {
//                     tokens.extend(
//                         syn::Error::new(
//                             self.expected.span(),
//                             "cannot use \"expected\" on a struct"
//                         ).to_compile_error()
//                     );
//                 }
//                 // note to self: don't call `self.span()` - it causes rustc to crash
//                 let (parse, peek, span) = field_to_tokens(
//                     data, Path::from_string("Self").unwrap()
//                 );
//                 tokens.extend(impl_ast_item(
//                     &quote!{}, &self.ident, &self.generics,
//                     parse,
//                     if self.no_peek { quote! { false } } else { peek },
//                     span,
//                     None
//                 ));
//             }
//             ast::Data::Enum(data) => {
//                 let mut parse_impl = quote! {};
//                 let mut peek_impl = quote! {};
//                 let mut children_impl = quote! {};
//                 for variant in data {
//                     let v = &variant.ident;
//                     if variant.fields.is_unit() {
//                         children_impl.extend(quote! { Self::#v => Default::default(), });
//                         // No peeking or parsing unit variants
//                     }
//                     else {
//                         let (parse, peek, _) = field_to_tokens(
//                             &variant.fields,
//                             Path::from_string(&format!("Self::{v}")).unwrap()
//                         );
//                         parse_impl.extend(quote! {
//                             if { #peek } {
//                                 return { #parse };
//                             }
//                         });
//                         peek_impl.extend(quote! {
//                             if { #peek } {
//                                 return true;
//                             }
//                         });
//                         let destruct;
//                         let mut names = quote! {};
//                         let mut children = quote! {};
//                         if variant.fields.is_struct() {
//                             for field in variant.fields.fields.iter() {
//                                 let name = &field.ident;
//                                 if field.skip || field.skip_with.is_some() {
//                                     names.extend(quote! { #name: _, });
//                                 }
//                                 else {
//                                     names.extend(quote! { #name, });
//                                     children.extend(quote! {
//                                         (#name as &dyn crate::checker::resolve::ResolveRef),
//                                     });
//                                 }
//                             }
//                             destruct = quote! { {#names} };
//                         }
//                         else {
//                             // todo: parse and peek impls
//                             for (field, c) in variant.fields.fields.iter().zip(
//                                 ('a'..='z').map(|c| Ident::new(&c.to_string(), v.span()))
//                             ) {
//                                 if field.skip || field.skip_with.is_some() {
//                                     names.extend(quote! { _, });
//                                 }
//                                 else {
//                                     names.extend(quote! { #c, });
//                                     children.extend(quote! {
//                                         (#c as &dyn crate::checker::resolve::ResolveRef),
//                                     });
//                                 }
//                             }
//                             destruct = quote! { (#names) };
//                         };
//                         children_impl.extend(quote! {
//                             Self::#v #destruct => vec![#children],
//                         });
//                     }
//                 }
                
//                 let expected = &self.expected;
//                 tokens.extend(impl_ast_item(
//                     &quote!{}, &self.ident, &self.generics,
//                     quote! {
//                         use crate::parser::parse::ParseRef;
//                         #parse_impl
//                         tokenizer.expected(#expected);
//                         Err(crate::parser::parse::FatalParseError)
//                     },
//                     if self.no_peek {
//                         quote! { false }
//                     }
//                     else {
//                         quote! {
//                             use crate::parser::parse::ParseRef;
//                             #peek_impl
//                             false
//                         }
//                     },
//                     quote! {
//                         use crate::parser::parse::Ref;
//                         match self {
//                             #children_impl
//                         }
//                     },
//                     None
//                 ));
//             }
//         }
//     }
// }

// #[proc_macro_derive(NodeKind, attributes(parse))]
// pub fn derive_node_kind(input: TokenStream) -> TokenStream {
//     match ParseReceiver::from_derive_input(&syn::parse(input).expect("Couldn't parse item")) {
//         Ok(v) => v,
//         Err(e) => {
//             return e.write_errors().into();
//         }
//     }.to_token_stream().into()
// }

// #[derive(FromDeriveInput)]
// #[darling(supports(enum_newtype))]
// struct ResolveReceiver {
//     ident: syn::Ident,
//     generics: syn::Generics,
//     data: ast::Data<ResolveVariant, ()>,
// }

// #[derive(FromVariant)]
// struct ResolveVariant {
//     ident: syn::Ident,
// }

// impl ToTokens for ResolveReceiver {
//     fn to_tokens(&self, tokens: &mut TokenStream2) {
//         let try_resolve;

//         match &self.data {
//             ast::Data::Struct(_) => {
//                 unimplemented!("structs not yet supported")
//             }
//             ast::Data::Enum(data) => {
//                 let mut try_resolve_matches = quote! {};
//                 for v in data {
//                     let ident = &v.ident;
//                     try_resolve_matches.extend(quote_spanned! {
//                         v.ident.span() =>
//                         Self::#ident(value) => crate::checker::resolve::ResolveRef::try_resolve_ref(value, pool, checker),
//                     });
//                 }
//                 try_resolve = quote! {
//                     match self {
//                         #try_resolve_matches
//                     }
//                 };
//             }
//         }

//         let name = &self.ident;
//         let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
//         tokens.extend(quote! {
//             impl #impl_generics crate::checker::resolve::ResolveNode for #name #ty_generics #where_clause {
//                 fn try_resolve_node(
//                     &mut self,
//                     pool: &crate::parser::parse::NodePool,
//                     checker: &mut crate::checker::coherency::Checker
//                 ) -> Option<crate::checker::ty::Ty> {
//                     #try_resolve
//                 }
//             }
//         });
//     }
// }

// #[proc_macro_derive(ResolveNode)]
// pub fn derive_resolve(input: TokenStream) -> TokenStream {
//     match ResolveReceiver::from_derive_input(&syn::parse(input).expect("Couldn't parse item")) {
//         Ok(v) => v,
//         Err(e) => {
//             return e.write_errors().into();
//         }
//     }.to_token_stream().into()
// }
