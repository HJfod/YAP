use crate::src::{Span, Src, SrcCursor, SrcID};
use std::fmt::Debug;
use std::fmt::Display;

use super::common::skip_c_like_comments;

pub trait DisplayName {
    /// Get the display name
    fn display_name(&self) -> String;
}

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
pub trait TokenKind: Debug + Sized + DisplayName {
    /// Get the next token in the source iterator. This method is **not required**
    /// to run [`TokenKind::skip_to_next`]; it may assume that the next token
    /// starts where the cursor is currently pointing at
    fn next(cursor: &mut SrcCursor) -> Token<Self>;

    /// Skip to the next token; by default skips whitespace and C-like comments
    /// (`// line comment` and `/* block comment */`)
    fn skip_to_next(cursor: &mut SrcCursor) {
        skip_c_like_comments(cursor);
    }
}

#[derive(Debug)]
pub enum ParsedTokenKind<T: TokenKind> {
    Token(T),
    EOF(Option<String>),
    Error(String),
}

#[derive(Debug)]
pub struct Token<T: TokenKind> {
    kind: ParsedTokenKind<T>,
    span: Span,
}

impl<T: TokenKind> Token<T> {
    pub fn new(kind: T, span: Span) -> Self {
        Self {
            kind: ParsedTokenKind::Token(kind),
            span,
        }
    }
    pub fn new_eof(name: Option<&str>, span: Span) -> Self {
        Self {
            kind: ParsedTokenKind::EOF(name.map(|n| n.into())),
            span,
        }
    }
    pub fn new_error<S: Display>(msg: S, span: Span) -> Self {
        Self {
            kind: ParsedTokenKind::Error(msg.to_string()),
            span,
        }
    }
    pub fn expected<W: Display, G: Display>(what: W, got: G, span: Span) -> Self {
        Self {
            kind: ParsedTokenKind::Error(format!("expected {what}, got {got}")),
            span,
        }
    }

    pub fn as_token(&self) -> Option<&T> {
        match &self.kind {
            ParsedTokenKind::Token(t) => Some(t),
            ParsedTokenKind::EOF(_) => None,
            ParsedTokenKind::Error(_) => None,
        }
    }
    pub fn as_eof(&self) -> Option<Option<&str>> {
        match &self.kind {
            ParsedTokenKind::Token(_) => None,
            ParsedTokenKind::EOF(s) => Some(s.as_deref()),
            ParsedTokenKind::Error(_) => None,
        }
    }
    pub fn as_error(&self) -> Option<&str> {
        match &self.kind {
            ParsedTokenKind::Token(_) => None,
            ParsedTokenKind::EOF(_) => None,
            ParsedTokenKind::Error(msg) => Some(msg),
        }
    }

    pub fn kind(&self) -> &ParsedTokenKind<T> {
        &self.kind
    }
    pub fn into_kind(self) -> ParsedTokenKind<T> {
        self.kind
    }
    pub fn into_token(self) -> Option<T> {
        match self.kind {
            ParsedTokenKind::Token(t) => Some(t),
            ParsedTokenKind::EOF(_) => None,
            ParsedTokenKind::Error(_) => None,
        }
    }
    pub fn into_eof(self) -> Option<Option<String>> {
        match self.kind {
            ParsedTokenKind::Token(_) => None,
            ParsedTokenKind::EOF(e) => Some(e),
            ParsedTokenKind::Error(_) => None,
        }
    }
    pub fn into_error(self) -> Option<String> {
        match self.kind {
            ParsedTokenKind::Token(_) => None,
            ParsedTokenKind::EOF(_) => None,
            ParsedTokenKind::Error(msg) => Some(msg),
        }
    }
    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, ParsedTokenKind::EOF(_))
    }
    pub fn is_error(&self) -> bool {
        matches!(self.kind, ParsedTokenKind::Error(_))
    }
}

impl<T: TokenKind> Display for Token<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParsedTokenKind::Token(t) => f.write_str(&t.display_name()),
            ParsedTokenKind::EOF(name) => {
                f.write_str(name.as_ref().map(|n| n.as_str()).unwrap_or("end-of-file"))
            }
            ParsedTokenKind::Error(msg) => {
                f.write_str(msg)
            }
        }
    }
}

/// Anything that can return Tokens should impl TokenIterator; which is not an
/// Iterator because it can never return None (and also so `peek` is not `mut`)
pub trait TokenIterator<T: TokenKind> {
    /// Get the next `Token` in the stream
    fn next(&mut self) -> Token<T>;
    /// Get the next upcoming `Token` in the stream without consuming it
    fn peek(&self) -> &Token<T>;
    /// Get the start position of the next `Token`
    fn start(&self) -> usize;
    /// Get the span from `start` to the end of the last token returned
    fn span_from(&self, start: usize) -> Span;
    /// Returns the name of the EOF token
    fn eof_name(&self) -> String;

    fn src(&self) -> SrcID;
}

/// Turns an unparsed source file into tokens
pub(crate) struct Tokenizer<'s, T: TokenKind> {
    cursor: SrcCursor<'s>,
    next: Token<T>,
    last_end: usize,
}
impl<'s, T: TokenKind> Tokenizer<'s, T> {
    fn fetch_next(cursor: &mut SrcCursor<'s>) -> Token<T> {
        T::skip_to_next(cursor);
        T::next(cursor)
    }
    pub fn new(src: &'s Src) -> Self {
        let mut cursor = src.cursor();
        Self {
            next: Self::fetch_next(&mut cursor),
            cursor,
            last_end: 0,
        }
    }
}
impl<T: TokenKind> TokenIterator<T> for Tokenizer<'_, T> {
    fn next(&mut self) -> Token<T> {
        self.last_end = self.next.span().end();
        std::mem::replace(&mut self.next, Self::fetch_next(&mut self.cursor))
    }
    fn peek(&self) -> &Token<T> {
        &self.next
    }
    fn start(&self) -> usize {
        self.next.span().start()
    }
    fn span_from(&self, start: usize) -> Span {
       self.cursor.src().span(start..self.last_end)
    }
    fn src(&self) -> SrcID {
        self.cursor.src().id()
    }
    fn eof_name(&self) -> String {
        String::from("end-of-file")
    }
}

#[derive(Debug)]
/// A list of pre-parsed tokens
pub struct TokenTree<T: TokenKind> {
    tokens: std::vec::IntoIter<Token<T>>,
    // Boxed because it's normal to include `TokenTree` in a `TokenKind`
    next: Box<Token<T>>,
    last_end: usize,
    eof: (Option<String>, Span),
}

impl<T: TokenKind> TokenTree<T> {
    fn fetch_next(
        iter: &mut std::vec::IntoIter<Token<T>>,
        (eof_name, eof_span): (Option<&str>, Span),
    ) -> Token<T> {
        iter.next().unwrap_or_else(|| Token::new_eof(eof_name, eof_span.clone()))
    }
    pub fn new(tokens: Vec<Token<T>>, eof: (Option<&str>, Span)) -> Self {
        let mut tokens = tokens.into_iter();
        Self {
            next: Box::from(Self::fetch_next(&mut tokens, eof.clone())),
            tokens,
            eof: (eof.0.map(|s| s.to_owned()), eof.1.clone()),
            last_end: 0,
        }
    }
    /// Consumes this tree and turns it into a vector of its contents
    pub fn into_items(self) -> Vec<Token<T>> {
        self.tokens.collect()
    }
}

impl<T: TokenKind> TokenIterator<T> for TokenTree<T> {
    fn next(&mut self) -> Token<T> {
        self.last_end = self.next.span().end();
        std::mem::replace(
            &mut self.next,
            Self::fetch_next(&mut self.tokens, (self.eof.0.as_deref(), self.eof.1.clone()))
        )
    }
    fn peek(&self) -> &Token<T> {
        &self.next
    }
    fn start(&self) -> usize {
        self.next.span().start()
    }
    fn span_from(&self, start: usize) -> Span {
       self.next.span().src().span(start..self.last_end)
    }
    fn eof_name(&self) -> String {
        self.eof.0.clone().unwrap_or(String::from("end-of-file"))
    }
    fn src(&self) -> SrcID {
        self.next.span().src()
    }
}
