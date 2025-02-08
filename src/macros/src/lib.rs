
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate darling;

use darling::ast::NestedMeta;
use darling::{ast, FromDeriveInput, FromField, FromMeta, FromVariant};
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{parse_macro_input, Fields, GenericArgument, Ident, ItemEnum, PathArguments, Type};
use syn::spanned::Spanned;

fn extract_node_type(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Path(p) => {
            if p.qself.is_some() {
                return None;
            }
            let path_as_str = p.path.segments.iter()
                .map(|p| p.ident.to_string())
                .collect::<Vec<_>>()
                .join("::");

            if !"prolangine::parse::node::Node".ends_with(&path_as_str) {
                return None;
            }
            match &p.path.segments.last().unwrap().arguments {
                PathArguments::AngleBracketed(ab) => {
                    match ab.args.first() {
                        Some(GenericArgument::Type(ty)) => {
                            Some(ty)
                        }
                        _ => None
                    }
                }
                _ => None
            }
        }
        _ => None,
    }
}

#[derive(FromDeriveInput)]
#[darling(attributes(parse), supports(any))]
struct ParseReceiver {
    ident: syn::Ident,
    generics: syn::Generics,
    data: ast::Data<ParseVariant, ParseField>,
    token_type: String,
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
}

#[derive(Default)]
struct ToImplForDerive {
    children_match_impl: TokenStream2,
    span_match_impl: TokenStream2,
    parse_impl: TokenStream2,
    peek_impl: TokenStream2,
}

impl ToImplForDerive {
    pub fn new_error<T: std::fmt::Display>(span: Span, message: T) -> Self {
        Self {
            children_match_impl: syn::Error::new(span, message).to_compile_error(),
            ..Default::default()
        }
    }
}

fn get_first_node_field_for_peeking(fields: &ast::Fields<ParseField>) -> Result<&Type, String> {
    // todo: handle optional fields
    match extract_node_type(&fields.iter().next().unwrap().ty) {
        Some(ty) => Ok(ty),
        None => Err("NodeKind variants must have at least one field of type `Node<Smth>`".into()),
    }
}

fn generate_field_parse_code(span: Span, ident: TokenStream2, fields: ast::Fields<ParseField>) -> ToImplForDerive {
    match fields.style {
        // Unit structs are disallowed because there's nothing to parse there
        ast::Style::Unit => {
            ToImplForDerive::new_error(span, "NodeKind variants must have be tuple or struct types")
        }

        // Tuple structs must be Thing(Node<X>) with one field only 
        // because if there's multiple nodes it's ambiguous whose 
        // Span to use so you need to add a Span and at that point 
        // just make it a struct for simplicity dawg
        ast::Style::Tuple => {
            if fields.len() != 1 {
                return ToImplForDerive::new_error(
                    span,
                    "NodeKind variants must either be tuples with exactly one \
                    Node<Smth> field or structs with a `span: Span` field"
                );
            }
            let first_ty = match get_first_node_field_for_peeking(&fields) {
                Ok(v) => v,
                Err(e) => return ToImplForDerive::new_error(span, e),
            };
            return ToImplForDerive {
                children_match_impl: quote! {
                    #ident (a) => a.children(),
                },
                span_match_impl: quote! {
                    #ident (a) => a.span(),
                },
                parse_impl: quote! {
                    Node::from(#ident(<#first_ty>::parse(tokenizer)))
                },
                peek_impl: quote! {
                    <#first_ty>::peek(tokenizer)
                },
            }
        }

        // Struct variants must have a `span: Span field`
        ast::Style::Struct => {
            if !fields.iter().any(|f| f.ident.as_ref().is_some_and(|i| i == "span")) {
                return ToImplForDerive::new_error(span, "NodeKind struct variants must have a `span: Span` field");
            }

            // Get the first type for peeking
            let first_ty = match get_first_node_field_for_peeking(&fields) {
                Ok(v) => v,
                Err(e) => return ToImplForDerive::new_error(span, e),
            };
    
            let mut destructure = quote! {};
            let mut children_result = quote! { let mut res = vec![]; };
            let mut parse_fields = quote! {};
            for v in fields.iter() {
                let ident = v.ident.as_ref().unwrap();

                // Destructuring just needs the name
                destructure.extend(quote! { #ident, });

                // If this type is a Node, deal with it like that
                if let Some(ty) = extract_node_type(&v.ty) {
                    children_result.extend(quote! {
                        res.extend(#ident.children());
                    });
                    parse_fields.extend(quote! {
                        #ident: Node::from(<#ty>::parse(tokenizer)),
                    });
                }
                // Span fields are parsed specially
                else if ident == "span" {
                    parse_fields.extend(quote! {
                        #ident: tokenizer.span_from(start),
                    });
                }
                // Otherwise use Default to construct whatever this field is
                else {
                    parse_fields.extend(quote! {
                        #ident: Default::default(),
                    });
                }
            }
            children_result.extend(quote! { res });
    
            ToImplForDerive {
                children_match_impl: quote! {
                    #[allow(unused_variables)]
                    #ident { #destructure } => {
                        #children_result
                    },
                },
                // We have already checked that there must exist a field named `span`
                span_match_impl: quote! {
                    #[allow(unused_variables)]
                    #ident { #destructure } => span.clone(),
                },
                parse_impl: quote! {
                    let start = tokenizer.start();
                    Node::from(#ident { #parse_fields })
                },
                peek_impl: quote! {
                    <#first_ty>::peek(tokenizer)
                },
            }
        }
    }
}

#[proc_macro_derive(NodeKind, attributes(parse))]
pub fn derive_node_kind(input: TokenStream) -> TokenStream {
    let item = match ParseReceiver::from_derive_input(&syn::parse(input).expect("Couldn't parse item")) {
        Ok(v) => v,
        Err(e) => { return e.write_errors().into(); }
    };

    let token_type = Ident::new(&item.token_type, item.ident.span());
    let for_item = &item.ident;

    let mut gen = ToImplForDerive::default();

    match item.data {
        ast::Data::Enum(variants) => {
            let Some(expected) = item.expected else {
                return syn::Error::new(
                    item.ident.span(),
                    "deriving NodeKind on an enum requires specifying #[parse(expected = \"\")]"
                ).to_compile_error().into();
            };
            for variant in variants {
                let ident = variant.ident;
                let vgen = generate_field_parse_code(ident.span(), quote! { Self::#ident }, variant.fields);
                gen.children_match_impl.extend(vgen.children_match_impl);
                gen.span_match_impl.extend(vgen.span_match_impl);

                let peek_impl = vgen.peek_impl;
                let parse_impl = vgen.parse_impl;
                gen.parse_impl.extend(quote! {
                    if #peek_impl {
                        #parse_impl
                    }
                    else
                });
                gen.peek_impl.extend(quote! { #peek_impl || });
            }
            // Normal case is error on next token
            gen.parse_impl.extend(quote! {
                {
                    let token = tokenizer.next();
                    Node::expected(#expected, &token, token.span())
                }
            });
            // Last case has a dangling ||
            gen.peek_impl.extend(quote! { false });
        }
        ast::Data::Struct(fields) => {
            let vgen = generate_field_parse_code(for_item.span(), quote! { Self }, fields);
            gen.children_match_impl.extend(vgen.children_match_impl);
            gen.span_match_impl.extend(vgen.span_match_impl);
            gen.parse_impl.extend(vgen.parse_impl);
            gen.peek_impl.extend(vgen.peek_impl);
        }
    }

    let parse_impl = gen.parse_impl;
    let peek_impl = gen.peek_impl;
    let children_match_impl = gen.children_match_impl;
    let span_match_impl = gen.span_match_impl;
    quote! {
        impl prolangine::parse::node::Parse<#token_type> for #for_item {
            fn parse<I>(tokenizer: &mut I) -> prolangine::parse::node::Node<Self>
                where
                    Self: Sized,
                    I: prolangine::parse::token::TokenIterator<#token_type>
            {
                #parse_impl
            }
            fn peek<I>(tokenizer: &I) -> bool
                where
                    Self: Sized,
                    I: prolangine::parse::token::TokenIterator<#token_type>
            {
                #peek_impl
            }
        }

        impl prolangine::parse::node::NodeKind for #for_item {
            fn children<'a>(&'a self) -> Vec<prolangine::parse::node::Node<dyn prolangine::parse::node::NodeKind + 'a>> {
                match self {
                    #children_match_impl
                }
            }
            fn span(&self) -> prolangine::src::Span {
                match self {
                    #span_match_impl
                }
            }
        }
    }.into()
}

#[derive(FromMeta)]
struct CreateTokenNodesArgs {}

#[derive(FromMeta)]
struct NestedTokenArgs {
    display_name: Option<String>,
    node: Option<String>,
}

#[proc_macro_attribute]
pub fn create_token_nodes(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => { return TokenStream::from(darling::Error::from(e).write_errors()); }
    };
    let _args = match CreateTokenNodesArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => { return TokenStream::from(e.write_errors()); }
    };
    let mut input = parse_macro_input!(input as ItemEnum);
    let token_vis = &input.vis;
    let token_name = &input.ident;
    let mut display_names = TokenStream2::new();

    let mut result = quote! {};
    for variant in &mut input.variants {
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
        let mut node_name = format_ident!("{}Token", variant.ident);
        
        let mut destructure_names = quote! {};
        let mut ty = quote! {};
        for (ix, field) in variant.fields.iter().enumerate() {
            let vty = &field.ty;
            if !ty.is_empty() {
                ty.extend(quote! { , });
            }
            ty.extend(quote! { #vty });

            if !destructure_names.is_empty() {
                destructure_names.extend(quote! { , });
            }
            let des = format_ident!("v_{ix}");
            destructure_names.extend(quote! { #des });
        }
        // Make the value type a tuple (or unit)
        if variant.fields.len() != 1 {
            destructure_names = quote! { (#destructure_names) };
            ty = quote! { (#ty) };
        }

        // On unit variants, we don't need to destructure in any way
        let destructure = match &variant.fields {
            Fields::Unnamed(_) => quote! { (#destructure_names) },
            Fields::Unit => quote! {},
            Fields::Named(_) => quote! {},
        };

        let mut display_name = variant_name.to_string().to_lowercase();

        let mut use_generic_parameter = false;

        // Parse #[token(...)] attributes & remove them from the output
        // `Vec::extract_if` is not stable...
        let mut i = 0;
        while i < variant.attrs.len() {
            if variant.attrs[i].meta.path().get_ident() == Some(&format_ident!("token")) {
                let attr = variant.attrs.remove(i);
                let args = match NestedTokenArgs::from_meta(&attr.meta) {
                    Ok(args) => args,
                    Err(e) => {
                        let t = e.write_errors();
                        result.extend(quote! { #t });
                        continue;
                    }
                };
                if let Some(n) = args.display_name {
                    display_name = n;
                }
                if let Some(n) = args.node {
                    if let Some(name) = n.strip_suffix("<T>") {
                        node_name = Ident::new(name, attr.span());
                        use_generic_parameter = true;
                    }
                    else {
                        node_name = Ident::new(&n, attr.span());
                        use_generic_parameter = false;
                    }
                }
            }
            else {
                i += 1;
            }
        }

        display_names.extend(quote! {
            #token_name::#variant_name #destructure => #display_name,
        });

        let generics_parse_def = use_generic_parameter.then(|| quote! {
            <N: prolangine::parse::node::NodeKind + prolangine::parse::node::Parse<#token_name>>
        }).unwrap_or_default();
        let generics_def = use_generic_parameter.then(|| quote! {
            <N: prolangine::parse::node::NodeKind>
        }).unwrap_or_default();
        let generics_use = use_generic_parameter.then(|| quote! { <N> }).unwrap_or_default();

        let parse_inner_impl = use_generic_parameter.then(|| quote! {
            #destructure_names.parse_fully_into()
        }).unwrap_or_else(|| quote! {
            #destructure_names
        });

        let children_impl = use_generic_parameter.then(|| quote! {
            vec![self.value.clone_dyn()]
        }).unwrap_or_else(|| quote! {
            vec![]
        });

        ty = use_generic_parameter.then(|| quote! { prolangine::parse::node::Node<N> }).unwrap_or(ty);

        result.extend(quote! {
            #[derive(Debug)]
            #token_vis struct #node_name #generics_def {
                value: #ty,
                span: prolangine::src::Span,
            }

            impl #generics_parse_def prolangine::parse::node::Parse<#token_name> for #node_name #generics_use {
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
                            value: #parse_inner_impl,
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

            impl #generics_def prolangine::parse::node::NodeKind for #node_name #generics_use {
                fn children<'a>(&'a self) -> Vec<prolangine::parse::node::Node<dyn NodeKind + 'a>> {
                    #children_impl
                }
                fn span(&self) -> Span {
                    self.span.clone()
                }
            }
        });
    }
    result.extend(quote! {
        #[derive(Debug)]
        #input

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
