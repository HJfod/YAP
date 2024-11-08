
use std::fmt::Debug;
use crate::{log::Logger, src::Span};

use super::token::{Token, TokenIterator, TokenKind};

pub trait NodeKind: Debug { 
    type TokenKind: TokenKind;

    fn parse<I>(tokenizer: &mut I, logger: &mut Logger) -> Self
        where
            Self: Sized,
            I: TokenIterator<Self::TokenKind>;

    /// Check if this type is posisbly coming up on the token stream at a position
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<Self::TokenKind>;

    /// Get the children of this AST node
    fn children(&self) -> Vec<&dyn NodeKind<TokenKind = Self::TokenKind>>;

    /// Get the span (raw source code string) of this node
    fn span(&self) -> Span;
}

impl<T: TokenKind> NodeKind for Token<T> {
    type TokenKind = T;
    fn parse<I>(tokenizer: &mut I, logger: &mut Logger) -> Self
        where
            Self: Sized,
            I: TokenIterator<Self::TokenKind>
    {
        tokenizer.next(logger)
    }
    fn peek<I>(_tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<Self::TokenKind>
    {
        true
    }
    fn children(&self) -> Vec<&dyn NodeKind<TokenKind = Self::TokenKind>> {
        vec![]
    }
    fn span(&self) -> Span {
        self.span()
    }
}

impl<T: NodeKind> NodeKind for Box<T> {
    type TokenKind = T::TokenKind;
    fn parse<I>(tokenizer: &mut I, logger: &mut Logger) -> Self
        where
            Self: Sized,
            I: TokenIterator<Self::TokenKind>
    {
        Box::from(T::parse(tokenizer, logger))
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<Self::TokenKind>
    {
        T::peek(tokenizer)
    }
    fn children(&self) -> Vec<&dyn NodeKind<TokenKind = Self::TokenKind>> {
        self.as_ref().children()
    }
    fn span(&self) -> Span {
        self.as_ref().span()
    }
}

#[derive(Debug)]
pub struct Maybe<N: NodeKind> {
    node: Option<N>,
    span: Span,
}

impl<N: NodeKind> NodeKind for Maybe<N> {
    type TokenKind = N::TokenKind;
    fn parse<I>(tokenizer: &mut I, logger: &mut Logger) -> Self
        where
            Self: Sized,
            I: TokenIterator<Self::TokenKind>
    {
        let start = tokenizer.start();
        Self {
            node: N::peek(tokenizer).then(|| N::parse(tokenizer, logger)),
            span: tokenizer.span_from(start),
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<Self::TokenKind>
    {
        N::peek(tokenizer)
    }

    fn children(&self) -> Vec<&dyn NodeKind<TokenKind = Self::TokenKind>> {
        self.node.as_ref()
            .map(|n| vec![n as &dyn NodeKind<TokenKind = Self::TokenKind>])
            .unwrap_or_default()
    }
    fn span(&self) -> Span {
        self.span.clone()
    }
}
