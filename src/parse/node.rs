
use std::fmt::{Debug, Display};
use crate::src::Span;
use super::token::{Token, TokenIterator, TokenKind};

pub trait NodeKind: Debug { 
    /// Get the children of this AST node
    fn children(&self) -> Vec<&dyn NodeKind>;

    /// Get the span (raw source code string) of this node
    fn span(&self) -> Span;
}

pub trait Parse<T: TokenKind>: NodeKind {
    fn parse<I>(tokenizer: &mut I) -> Node<Self>
        where
            Self: Sized,
            I: TokenIterator<T>;

    /// Check if this type is posisbly coming up on the token stream at a position
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<T>;
}

#[derive(Debug)]
pub enum Node<N: NodeKind> {
    Some(Box<N>),
    Error(String, Span),
}

impl<N: NodeKind> Node<N> {
    pub fn expected<W: Display, G: Display>(what: W, got: G, span: Span) -> Self {
        Self::Error(format!("expected {what}, got {got}"), span)
    }
}

impl<N: NodeKind> From<N> for Node<N> {
    fn from(node: N) -> Self {
        Self::Some(Box::from(node))
    }
}

impl<N: NodeKind> NodeKind for Node<N> {
    fn children(&self) -> Vec<&dyn NodeKind> {
        match self {
            Self::Some(o) => o.children(),
            Self::Error(_, _) => vec![],
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::Some(o) => o.span(),
            Self::Error(_, span) => span.clone(),
        }
    }
}

impl<T: TokenKind> Parse<T> for Token<T> {
    fn parse<I>(tokenizer: &mut I) -> Node<Self>
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        Node::Some(Box::from(tokenizer.next()))
    }
    fn peek<I>(_tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        // The stream will always contain more tokens because EOF is a token
        true
    }
}
impl<T: TokenKind> NodeKind for Token<T> {
    fn children(&self) -> Vec<&dyn NodeKind> {
        vec![]
    }
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub enum Maybe<N: NodeKind> {
    Some(Node<N>),
    None(Span),
}

impl<T: TokenKind, N: NodeKind + Parse<T>> Parse<T> for Maybe<N> {
    fn parse<I>(tokenizer: &mut I) -> Node<Self>
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            Node::from(Self::Some(N::parse(tokenizer)))
        }
        else {
            Node::from(Self::None(tokenizer.span_from(start)))
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        N::peek(tokenizer)
    }
}
impl<N: NodeKind> NodeKind for Maybe<N> {
    fn children(&self) -> Vec<&dyn NodeKind> {
        match self {
            Self::Some(n) => n.children(),
            Self::None(_) => vec![],
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::Some(n) => n.span(),
            Self::None(span) => span.clone(),
        }
    }
}
