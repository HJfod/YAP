
use std::fmt::Debug;
use crate::src::Span;
use super::token::{Error, ParseResult, Token, TokenIterator, TokenKind};

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
    fn tokenize_fully(self) -> ParseResult<Vec<Token<T>>>;
    fn parse<N: Parse<T>>(&mut self) -> ParseResult<N>;
    fn parse_fully<N: Parse<T>>(self) -> ParseResult<N>;
}

impl<T: TokenKind, I: TokenIterator<T>> Parser<T> for I {
    fn tokenize_fully(mut self) -> ParseResult<Vec<Token<T>>> {
        let mut res = Vec::new();
        let mut errors = Vec::new();
        loop {
            match self.next() {
                Ok(token) => {
                    if token.is_eof() {
                        break;
                    }
                    // If we have ran into any errors, it's pointless to keep collecting 
                    // up results
                    if errors.is_empty() {
                        res.push(token);
                    }
                }
                Err(e) => errors.extend(e),
            }
        }
        if errors.is_empty() {
            Ok(res)
        }
        else {
            Err(errors)
        }
    }
    fn parse<N: Parse<T>>(&mut self) -> ParseResult<N> {
        N::parse(self)
    }
    fn parse_fully<N: Parse<T>>(mut self) -> ParseResult<N> {
        let res = N::parse(&mut self);
        if let Ok(next) = self.next() {
            if !next.is_eof() {
                let span = next.span();
                let mut errors = match res {
                    Ok(_) => vec![],
                    Err(e) => e,
                };
                errors.push(Error::expected(self.eof_name(), next, span));
                return Err(errors);
            }
        }
        res
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

impl<N: NodeKind> NodeKind for Box<N> {
    fn children(&self) -> Vec<&dyn NodeKind> {
        self.as_ref().children()
    }
    fn span(&self) -> Span {
        self.as_ref().span()
    }
}
impl<T: TokenKind, N: Parse<T>> Parse<T> for Box<N> {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        Ok(Box::from(N::parse(tokenizer)?))
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        N::peek(tokenizer)
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
impl<T: TokenKind> Parse<T> for Token<T> {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
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
