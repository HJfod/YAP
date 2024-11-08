
use std::fmt::Debug;
use crate::{log::Logger, src::Span};

use super::token::{Token, TokenIterator, TokenKind};

pub trait NodeKind<'s>: Debug { 
    type TokenKind: TokenKind<'s>;

    fn parse<I>(tokenizer: &mut I, logger: &mut Logger<'s>) -> Self
        where
            Self: Sized,
            I: TokenIterator<'s, Self::TokenKind>;

    /// Check if this type is posisbly coming up on the token stream at a position
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<'s, Self::TokenKind>;

    /// Get the children of this AST node
    fn children(&self) -> Vec<&dyn NodeKind<'s, TokenKind = Self::TokenKind>>;

    /// Get the span (raw source code string) of this node
    fn span(&self) -> Span<'s>;
}

impl<'s, T: TokenKind<'s>> NodeKind<'s> for Token<'s, T> {
    type TokenKind = T;
    fn parse<I>(tokenizer: &mut I, logger: &mut Logger<'s>) -> Self
        where
            Self: Sized,
            I: TokenIterator<'s, Self::TokenKind>
    {
        tokenizer.next(logger)
    }
    fn peek<I>(_tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<'s, Self::TokenKind>
    {
        true
    }
    fn children(&self) -> Vec<&dyn NodeKind<'s, TokenKind = Self::TokenKind>> {
        vec![]
    }
    fn span(&self) -> Span<'s> {
        self.span()
    }
}

impl<'s, T: NodeKind<'s>> NodeKind<'s> for Box<T> {
    type TokenKind = T::TokenKind;
    fn parse<I>(tokenizer: &mut I, logger: &mut Logger<'s>) -> Self
        where
            Self: Sized,
            I: TokenIterator<'s, Self::TokenKind>
    {
        Box::from(T::parse(tokenizer, logger))
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<'s, Self::TokenKind>
    {
        T::peek(tokenizer)
    }
    fn children(&self) -> Vec<&dyn NodeKind<'s, TokenKind = Self::TokenKind>> {
        self.as_ref().children()
    }
    fn span(&self) -> Span<'s> {
        self.as_ref().span()
    }
}

#[derive(Debug)]
pub struct Maybe<'s, N: NodeKind<'s>> {
    node: Option<N>,
    span: Span<'s>,
}

impl<'s, N: NodeKind<'s>> NodeKind<'s> for Maybe<'s, N> {
    type TokenKind = N::TokenKind;
    fn parse<I>(tokenizer: &mut I, logger: &mut Logger<'s>) -> Self
        where
            Self: Sized,
            I: TokenIterator<'s, Self::TokenKind>
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
            I: TokenIterator<'s, Self::TokenKind>
    {
        N::peek(tokenizer)
    }

    fn children(&self) -> Vec<&dyn NodeKind<'s, TokenKind = Self::TokenKind>> {
        self.node.as_ref()
            .map(|n| vec![n as &dyn NodeKind<'s, TokenKind = Self::TokenKind>])
            .unwrap_or_default()
    }
    fn span(&self) -> Span<'s> {
        self.span.clone()
    }
}
