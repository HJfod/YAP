
use std::fmt::{Debug, Display};
use crate::src::Span;
use super::token::{Token, TokenIterator, TokenKind};

impl Error {
    pub fn new<W: Display>(msg: W, span: Span) -> Self {
        Self { message: msg.to_string(), span }
    }
    pub fn expected<W: Display, G: Display>(what: W, got: G, span: Span) -> Self {
        Self { message: format!("expected {what}, got {got}"), span }
    }
}

pub trait NodeKind: Debug {
    /// Get the children of this AST node
    fn children(&self) -> Vec<&dyn NodeKind>;
    /// Get the span (raw source code string) of this node
    fn span(&self) -> Span;
}

pub trait Parse<T: TokenKind>: NodeKind {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
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
    fn parse<N: Parse<T>>(&mut self) -> ParseResult<N>;
    fn parse_fully<N: Parse<T>>(self) -> Result<N, Vec<Error>>;
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
    fn parse<N: Parse<T>>(&mut self) -> ParseResult<N> {
        N::parse(self)
    }
    fn parse_fully<N: Parse<T>>(mut self) -> Result<N, Error> {
        let res = N::parse(&mut self);
        let next = self.next();
        if !next.is_eof() {
            let span = next.span();
            return Err(Error::expected(self.eof_name(), next, span));
        }
        Ok(res)
    }
}

// #[derive(Debug)]
// pub enum Node<N: NodeKind + ?Sized> {
//     Some(Rc<N>),
//     Error(String, Span),
// }

// impl<N: NodeKind + ?Sized> Node<N> {
//     pub fn contains_errors(&self) -> bool {
//         match self {
//             Node::Some(n) => n.children().into_iter().any(|n| n.contains_errors()),
//             Node::Error(_, _) => true,
//         }
//     }
//     pub fn get_errors(&self) -> Vec<(String, Span)> {
//         let mut res = Vec::new();
//         match self {
//             Node::Some(n) => n.children().into_iter().for_each(|c| res.extend(c.get_errors())),
//             Node::Error(e, s) => res.push((e.clone(), s.clone())),
//         }
//         res
//     }
// }
// impl<N: NodeKind> Node<N> {
//     pub fn clone_dyn<'a>(&'a self) -> Node<dyn NodeKind + 'a> {
//         match self.clone() {
//             Self::Some(n) => Node::Some(n as Rc<dyn NodeKind + 'a>),
//             Self::Error(e, s) => Node::Error(e, s),
//         }
//     }
// }

// impl<N: NodeKind + ?Sized> Clone for Node<N> {
//     fn clone(&self) -> Self {
//         match self {
//             Node::Some(n) => Self::Some(Rc::clone(n)),
//             Node::Error(e, s) => Self::Error(e.clone(), s.clone()),
//         }
//     }
// }
// impl<N: NodeKind> From<N> for Node<N> {
//     fn from(node: N) -> Self {
//         Self::Some(Rc::from(node))
//     }
// }

// impl<N: NodeKind + ?Sized> NodeKind for Node<N> {
//     fn children<'a>(&'a self) -> Vec<Node<dyn NodeKind + 'a>> {
//         match self {
//             Self::Some(o) => o.children(),
//             Self::Error(_, _) => vec![],
//         }
//     }
//     fn span(&self) -> Span {
//         match self {
//             Self::Some(o) => o.span(),
//             Self::Error(_, span) => span.clone(),
//         }
//     }
// }

impl<T: TokenKind> Parse<T> for Token<T> {
    fn parse<I>(tokenizer: &mut I) -> Self
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        tokenizer.next()
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
    fn errors(&self) -> Vec<Error> {
        if let Some(err) = self.as_error() {
            vec![Error::new(err, self.span())]
        }
        else {
            vec![]
        }
    }
}

#[derive(Debug)]
pub enum Maybe<N: NodeKind> {
    Some(N),
    None(Span),
}

impl<T: TokenKind, N: NodeKind + Parse<T>> Parse<T> for Maybe<N> {
    fn parse<I>(tokenizer: &mut I) -> Self
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            Self::Some(N::parse(tokenizer))
        }
        else {
            Self::None(tokenizer.span_from(start))
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
    fn errors(&self) -> Vec<Error> {
        match self {
            Self::Some(n) => n.errors(),
            Self::None(_) => vec![],
        }
    }
}

/// Zero or more of specific type of node
#[derive(Debug)]
pub struct ZeroOrMore<N: NodeKind> {
    items: Vec<N>,
    span: Span,
}

impl<N: NodeKind> NodeKind for ZeroOrMore<N> {
    fn children(&self) -> Vec<&dyn NodeKind> {
        self.items.iter().map(|n| n as &dyn NodeKind).collect()
    }
    fn span(&self) -> Span {
        self.span.clone()
    }
    fn errors(&self) -> Vec<Error> {
        self.items.iter().flat_map(|i| i.errors()).collect()
    }
}
impl<T: TokenKind, N: NodeKind + Parse<T>> Parse<T> for ZeroOrMore<N> {
    fn parse<I>(tokenizer: &mut I) -> Self
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        let mut items = vec![];
        while N::peek(tokenizer) {
            items.push(N::parse(tokenizer));
        }
        Self {
            items,
            span: tokenizer.span_from(start),
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
