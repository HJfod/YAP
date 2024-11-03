
use std::fmt::Debug;
use crate::{lang::Language, src::Span};

use super::token::TokenIterator;

pub trait NodeKind<'s, L: Language>: Debug {
    fn parse<T>(tokenizer: &mut T) -> Self
        where
            Self: Sized,
            T: TokenIterator<'s, L>;

    /// Check if this type is posisbly coming up on the token stream at a position
    fn peek<T>(tokenizer: &T) -> bool
        where
            Self: Sized,
            T: TokenIterator<'s, L>;

    /// Get the children of this AST node
    fn children(&self) -> Vec<&dyn NodeKind<'s, L>>;

    /// Get the span (raw source code string) of this node
    fn span(&self) -> Span<'s, L>;
}

#[derive(Debug)]
pub struct Maybe<'s, L: Language, N: NodeKind<'s, L>> {
    node: Option<N>,
    span: Span<'s, L>,
}

impl<'s, L: Language, N: NodeKind<'s, L>> NodeKind<'s, L> for Maybe<'s, L, N> {
    fn parse<T>(tokenizer: &mut T) -> Self
        where
            Self: Sized,
            T: TokenIterator<'s, L>
    {
        let start = tokenizer.start();
        Self {
            node: N::peek(tokenizer).then(|| N::parse(tokenizer)),
            span: tokenizer.span_from(start),
        }
    }
    fn peek<T>(tokenizer: &T) -> bool
        where
            Self: Sized,
            T: TokenIterator<'s, L>
    {
        N::peek(tokenizer)
    }

    fn children(&self) -> Vec<&dyn NodeKind<'s, L>> {
        self.node.as_ref()
            .map(|n| vec![n as &dyn NodeKind<'s, L>])
            .unwrap_or_default()
    }
    fn span(&self) -> Span<'s, L> {
        self.span.clone()
    }
}
