use crate::src::{Span, Src, SrcCursor};
use std::fmt::Debug;
use std::{fmt::Display, marker::PhantomData};

use super::common::skip_c_like_comments;

/// The main trait for introducing a custom syntax for a language. The
/// language's token kind should implement this trait.
///
/// Unrecoverable token parsing errors and EOF are considered tokens
/// aswell for uniform handling.
///
/// The error token type must be able to encode the error message, and the EOF
/// token type must be able to encode an optional name for the EOF (such as
/// "closing brace"), since EOF does not necessarily mean end-of-file but can
/// also mean end-of-subtree
pub trait TokenKind<'s>: Debug + Sized {
    /// Get the display name for this token
    fn display_name(&self) -> String;

    /// Get the next token in the source iterator. This method is **not required**
    /// to run [`TokenKind::skip_to_next`]; it may assume that the next token
    /// starts where the cursor is currently pointing at
    fn next(cursor: &mut SrcCursor<'s>) -> Token<'s, Self>;

    /// Skip to the next token; by default skips whitespace and C-like comments
    /// (`// line comment` and `/* block comment */`)
    fn skip_to_next(cursor: &mut SrcCursor) {
        skip_c_like_comments(cursor);
    }
}

#[derive(Debug)]
pub enum ParsedTokenKind<'s, T: TokenKind<'s>> {
    Token(T, PhantomData<&'s ()>),
    EOF(Option<String>),
    Error(String),
}

#[derive(Debug)]
pub struct Token<'s, T: TokenKind<'s>> {
    kind: ParsedTokenKind<'s, T>,
    raw: &'s str,
    span: Span<'s>,
}

impl<'s, T: TokenKind<'s>> Token<'s, T> {
    pub fn new(kind: T, span: Span<'s>) -> Self {
        Self {
            kind: ParsedTokenKind::Token(kind, PhantomData),
            raw: span.data(),
            span,
        }
    }
    pub fn new_eof(name: Option<&str>, span: Span<'s>) -> Self {
        Self {
            kind: ParsedTokenKind::EOF(name.map(|n| n.into())),
            raw: span.data(),
            span,
        }
    }
    pub fn new_error(msg: &str, span: Span<'s>) -> Self {
        Self {
            kind: ParsedTokenKind::Error(msg.into()),
            raw: span.data(),
            span,
        }
    }

    pub fn kind(&self) -> &ParsedTokenKind<'s, T> {
        &self.kind
    }
    pub fn raw(&self) -> &'s str {
        self.raw
    }
    pub fn span(&self) -> Span<'s> {
        self.span.clone()
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, ParsedTokenKind::EOF(_))
    }
    pub fn is_error(&self) -> bool {
        matches!(self.kind, ParsedTokenKind::Error(_))
    }

    pub fn eof_name(&self) -> Option<&str> {
        if let ParsedTokenKind::EOF(Some(ref name)) = self.kind {
            Some(name.as_str())
        }
        else {
            None
        }
    }
    pub fn error_msg(&self) -> Option<&str> {
        if let ParsedTokenKind::Error(ref msg) = self.kind {
            Some(msg.as_str())
        }
        else {
            None
        }
    }
}

impl<'s, T: TokenKind<'s>> Display for Token<'s, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParsedTokenKind::Token(t, _) => f.write_str(&t.display_name()),
            ParsedTokenKind::EOF(name) => {
                f.write_str(name.as_ref().map(|n| n.as_str()).unwrap_or("end-of-file"))
            }
            ParsedTokenKind::Error(error) => f.write_str(error),
        }
    }
}

pub fn parse_fully_into_tokens<'s, T>(src: &'s Src) -> Vec<Token<'s, T>>
where
    T: TokenKind<'s>,
{
    let mut tokenizer = Tokenizer::<T>::new(src);
    let mut res = Vec::new();
    loop {
        let token = tokenizer.next();
        if token.is_eof() {
            break;
        }
        res.push(token);
    }
    res
}

/// Anything that can return Tokens should impl TokenIterator; which is not an
/// Iterator because it can never return None
pub(crate) trait TokenIterator<'s, T: TokenKind<'s>> {
    fn next(&mut self) -> Token<'s, T>;
}

/// Turns an unparsed source file into tokens
pub(crate) struct Tokenizer<'s, T: TokenKind<'s>> {
    cursor: SrcCursor<'s>,
    _phantom: PhantomData<T>,
}
impl<'s, T: TokenKind<'s>> Tokenizer<'s, T> {
    pub fn new(src: &'s Src) -> Self {
        Self {
            cursor: src.cursor(),
            _phantom: PhantomData,
        }
    }
}
impl<'s, T: TokenKind<'s>> TokenIterator<'s, T> for Tokenizer<'s, T> {
    fn next(&mut self) -> Token<'s, T> {
        T::skip_to_next(&mut self.cursor);
        T::next(&mut self.cursor)
    }
}

#[derive(Debug)]
/// A list of pre-parsed tokens
pub struct TokenTree<'s, T: TokenKind<'s>> {
    tokens: std::vec::IntoIter<Token<'s, T>>,
    eof_span: Span<'s>,
}

impl<'s, T: TokenKind<'s>> TokenTree<'s, T> {
    pub fn new(tokens: Vec<Token<'s, T>>, eof_span: Span<'s>) -> Self {
        Self {
            tokens: tokens.into_iter(),
            eof_span,
        }
    }
}

impl<'s, T: TokenKind<'s>> TokenIterator<'s, T> for TokenTree<'s, T> {
    fn next(&mut self) -> Token<'s, T> {
        self.tokens
            .next()
            .unwrap_or(Token::new_eof(None, self.eof_span.clone()))
    }
}
