
use std::fmt::Debug;
use crate::src::Span;

use super::token::{TokenIterator, TokenKind};

pub trait NodeKind<'s>: Debug {
    fn parse<T, I>(tokenizer: &mut I) -> Self
        where
            Self: Sized,
            T: TokenKind<'s>,
            I: TokenIterator<'s, T>;

    /// Check if this type is posisbly coming up on the token stream at a position
    fn peek<T, I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            T: TokenKind<'s>,
            I: TokenIterator<'s, T>;

    /// Get the children of this AST node
    fn children(&self) -> Vec<&dyn NodeKind<'s>>;

    /// Get the span (raw source code string) of this node
    fn span(&self) -> Span<'s>;
}

#[derive(Debug)]
pub struct Maybe<'s, N: NodeKind<'s>> {
    node: Option<N>,
    span: Span<'s>,
}

impl<'s, N: NodeKind<'s>> NodeKind<'s> for Maybe<'s, N> {
    fn parse<T, I>(tokenizer: &mut I) -> Self
        where
            Self: Sized,
            T: TokenKind<'s>,
            I: TokenIterator<'s, T>
    {
        let start = tokenizer.start();
        Self {
            node: N::peek(tokenizer).then(|| N::parse(tokenizer)),
            span: tokenizer.span_from(start),
        }
    }
    fn peek<T, I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            T: TokenKind<'s>,
            I: TokenIterator<'s, T>
    {
        N::peek(tokenizer)
    }

    fn children(&self) -> Vec<&dyn NodeKind<'s>> {
        self.node.as_ref()
            .map(|n| vec![n as &dyn NodeKind<'s>])
            .unwrap_or_default()
    }
    fn span(&self) -> Span<'s> {
        self.span.clone()
    }
}
