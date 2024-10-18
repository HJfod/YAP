
use std::fmt::Debug;
use crate::{lang::Language, src::Span};

pub trait NodeKind<'s>: Debug {
    type Language: Language;

    /// Get the children of this AST node
    fn children(&self) -> Vec<Box<dyn NodeKind<'s, Language = Self::Language>>>;

    /// Get the span (raw source code string) of this node
    fn span(&self) -> Span<'s, Self::Language>;
}

#[derive(Debug)]
pub struct Maybe<'s, N: NodeKind<'s>> {
    node: Option<N>,
    span: Span<'s, N::Language>,
}

impl<'s, N: NodeKind<'s>> NodeKind<'s> for Maybe<'s, N> {
    type Language = N::Language;
    fn children(&self) -> Vec<Box<dyn NodeKind<'s, Language = Self::Language>>> {
        self.node.as_ref().map(|n| n.children()).unwrap_or_default()
    }
    fn span(&self) -> Span<'s, Self::Language> {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct List<'s, N: NodeKind<'s>> {
    list: Vec<N>,
    span: Span<'s, N::Language>,
}

impl<'s, N: NodeKind<'s>> NodeKind<'s> for List<'s, N> {
    type Language = N::Language;
    fn children(&self) -> Vec<Box<dyn NodeKind<'s, Language = Self::Language>>> {
        self.list.iter().flat_map(|n| n.children()).collect()
    }
    fn span(&self) -> Span<'s, Self::Language> {
        self.span.clone()
    }
}
