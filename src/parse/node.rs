
use std::{fmt::{Debug, Display}, rc::Rc};
use crate::src::Span;
use super::token::{Token, TokenIterator, TokenKind};

// todo: maybe errors should be stored in the nodes instead?
// pros:
// + not every node can have errors (for example any errors in Expr are either 
//   "expected expression" or a failure to parse one of its enum variants)
// + some nodes can have errors but still be meaningfully usable (for example 
//   string literals with invalid escape sequences)
// + some nodes may have multiple errors at once but the current design only 
//   supports having one
// + no need to deal with the awkwardness of Nodes being possibly invalid
// cons:
// - actually it's good to deal with the awkwardness of Nodes being possibly 
//   invalid because if the Node has run into an unparseable error then we 
//   know we shouldn't even attempt to process it further and fabricating 
//   false Nodes could cause more errors down the line (like the stupid 
//   'implicit int' errors in C/C++)
// - now basically every token has to carry Option<(String, Span)> alongside it

pub trait NodeKind: Debug { 
    /// Get the children of this AST node
    fn children<'a>(&'a self) -> Vec<Node<dyn NodeKind + 'a>>;

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

pub trait Parser<T: TokenKind> {
    fn tokenize_fully(self) -> Vec<Token<T>>;
    fn parse<N: Parse<T>>(&mut self) -> Node<N>;
    fn parse_fully<N: Parse<T>>(self) -> Node<N>;
}

impl<T: TokenKind, I: TokenIterator<T>> Parser<T> for I {
    fn tokenize_fully(mut self) -> Vec<Token<T>> {
        let mut res = Vec::new();
        loop {
            let token = self.next();
            if token.is_eof() {
                break;
            }
            res.push(token);
        }
        res
    }
    fn parse<N: Parse<T>>(&mut self) -> Node<N> {
        N::parse(self)
    }
    fn parse_fully<N: Parse<T>>(mut self) -> Node<N> {
        let res = N::parse(&mut self);
        let next = self.next();
        if !next.is_eof() {
            let span = next.span();
            // todo: this is probably bad that it returns an error node instead of the parse tree
            return Node::expected(self.eof_name(), next, span);
        }
        res
    }
}

#[derive(Debug)]
pub enum Node<N: NodeKind + ?Sized> {
    Some(Rc<N>),
    Error(String, Span),
}

impl<N: NodeKind + ?Sized> Node<N> {
    pub fn expected<W: Display, G: Display>(what: W, got: G, span: Span) -> Self {
        Self::Error(format!("expected {what}, got {got}"), span)
    }
    pub fn contains_errors(&self) -> bool {
        match self {
            Node::Some(n) => n.children().into_iter().any(|n| n.contains_errors()),
            Node::Error(_, _) => true,
        }
    }
    pub fn get_errors(&self) -> Vec<(String, Span)> {
        let mut res = Vec::new();
        match self {
            Node::Some(n) => n.children().into_iter().for_each(|c| res.extend(c.get_errors())),
            Node::Error(e, s) => res.push((e.clone(), s.clone())),
        }
        res
    }
}
impl<N: NodeKind> Node<N> {
    pub fn clone_dyn<'a>(&'a self) -> Node<dyn NodeKind + 'a> {
        match self.clone() {
            Self::Some(n) => Node::Some(n as Rc<dyn NodeKind + 'a>),
            Self::Error(e, s) => Node::Error(e, s),
        }
    }
}

impl<N: NodeKind + ?Sized> Clone for Node<N> {
    fn clone(&self) -> Self {
        match self {
            Node::Some(n) => Self::Some(Rc::clone(n)),
            Node::Error(e, s) => Self::Error(e.clone(), s.clone()),
        }
    }
}
impl<N: NodeKind> From<N> for Node<N> {
    fn from(node: N) -> Self {
        Self::Some(Rc::from(node))
    }
}

impl<N: NodeKind + ?Sized> NodeKind for Node<N> {
    fn children<'a>(&'a self) -> Vec<Node<dyn NodeKind + 'a>> {
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
        Node::Some(Rc::from(tokenizer.next()))
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
    fn children(&self) -> Vec<Node<dyn NodeKind>> {
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
    fn children<'a>(&'a self) -> Vec<Node<dyn NodeKind + 'a>> {
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
