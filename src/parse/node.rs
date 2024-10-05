
use std::fmt::Debug;
use crate::{lang::Language, src::{overall_span, Span}};

pub trait NodeKind<'s, L: Language>: Debug {
    /// Get the children of this AST node
    fn children(&self) -> Vec<Box<dyn NodeKind<'s, L>>>;

    /// Get the span (raw source code string) of this node
    fn span(&self) -> Option<Span<'s, L>>;
}

impl<'s, L: Language, N: NodeKind<'s, L>> NodeKind<'s, L> for Option<N> {
    fn children(&self) -> Vec<Box<dyn NodeKind<'s, L>>> {
        self.as_ref().map(|n| n.children()).unwrap_or_default()
    }
    fn span(&self) -> Option<Span<'s, L>> {
        self.as_ref().and_then(|n| n.span())
    }
}
impl<'s, L: Language, N: NodeKind<'s, L>> NodeKind<'s, L> for Vec<N> {
    fn children(&self) -> Vec<Box<dyn NodeKind<'s, L>>> {
        self.iter().flat_map(|n| n.children()).collect()
    }
    fn span(&self) -> Option<Span<'s, L>> {
        overall_span(self.iter().flat_map(|n| n.span()))
    }
}
