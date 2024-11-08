use crate::log::{Level, Logger, Message};
use crate::src::{Span, Src, SrcCursor};
use std::fmt::Debug;
use std::fmt::Display;
use std::marker::PhantomData;

use super::common::skip_c_like_comments;
use super::node::NodeKind;

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
    fn next(cursor: &mut SrcCursor<'s>, logger: &mut Logger<'s>) -> Token<'s, Self>;

    /// Skip to the next token; by default skips whitespace and C-like comments
    /// (`// line comment` and `/* block comment */`)
    fn skip_to_next(cursor: &mut SrcCursor<'s>) {
        skip_c_like_comments(cursor);
    }
}

#[derive(Debug)]
pub enum ParsedTokenKind<'s, T: TokenKind<'s>> {
    Token(T, PhantomData<&'s Src>),
    EOF(Option<String>),
}

#[derive(Debug)]
pub struct Token<'s, T: TokenKind<'s>> {
    kind: ParsedTokenKind<'s, T>,
    span: Span<'s>,
}

impl<'s, T: TokenKind<'s>> Token<'s, T> {
    pub fn new(kind: T, span: Span<'s>) -> Self {
        Self {
            kind: ParsedTokenKind::Token(kind, PhantomData),
            span,
        }
    }
    pub fn new_eof(name: Option<&str>, span: Span<'s>) -> Self {
        Self {
            kind: ParsedTokenKind::EOF(name.map(|n| n.into())),
            span,
        }
    }

    pub fn as_token(&self) -> Option<&T> {
        match &self.kind {
            ParsedTokenKind::Token(t, _) => Some(t),
            ParsedTokenKind::EOF(_) => None,
        }
    }
    pub fn as_eof(&self) -> Option<Option<&str>> {
        match &self.kind {
            ParsedTokenKind::Token(_, _) => None,
            ParsedTokenKind::EOF(s) => Some(s.as_deref()),
        }
    }

    pub fn kind(&self) -> &ParsedTokenKind<'s, T> {
        &self.kind
    }
    pub fn into_kind(self) -> ParsedTokenKind<'s, T> {
        self.kind
    }
    pub fn into_token(self) -> Option<T> {
        match self.kind {
            ParsedTokenKind::Token(t, _) => Some(t),
            ParsedTokenKind::EOF(_) => None,
        }
    }
    pub fn into_eof(self) -> Option<Option<String>> {
        match self.kind {
            ParsedTokenKind::Token(_, _) => None,
            ParsedTokenKind::EOF(e) => Some(e),
        }
    }
    pub fn span(&self) -> Span<'s> {
        self.span.clone()
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, ParsedTokenKind::EOF(_))
    }

    pub fn eof_name(&self) -> Option<&str> {
        if let ParsedTokenKind::EOF(Some(ref name)) = self.kind {
            Some(name.as_str())
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
        }
    }
}

/// Anything that can return Tokens should impl TokenIterator; which is not an
/// Iterator because it can never return None (and also so `peek` is not `mut`)
pub trait TokenIterator<'s, T: TokenKind<'s>> {
    /// Get the next `Token` in the stream
    fn next(&mut self, logger: &mut Logger<'s>) -> Token<'s, T>;
    /// Get the next upcoming `Token` in the stream without consuming it
    fn peek(&self) -> &Token<'s, T>;
    /// Get the start position of the next `Token`
    fn start(&self) -> usize;
    /// Get the span from `start` to the end of the last token returned
    fn span_from(&self, start: usize) -> Span<'s>;

    fn src(&self) -> &'s Src;
}

/// Turns an unparsed source file into tokens
pub(crate) struct Tokenizer<'s, T: TokenKind<'s>> {
    cursor: SrcCursor<'s>,
    next: Token<'s, T>,
    last_end: usize,
}
impl<'s, T: TokenKind<'s>> Tokenizer<'s, T> {
    fn fetch_next(cursor: &mut SrcCursor<'s>, logger: &mut Logger<'s>) -> Token<'s, T> {
        T::skip_to_next(cursor);
        T::next(cursor, logger)
    }
    pub fn new(src: &'s Src, logger: &mut Logger<'s>) -> Self {
        let mut cursor = src.cursor();
        Self {
            next: Self::fetch_next(&mut cursor, logger),
            cursor,
            last_end: 0,
        }
    }
}
impl<'s, T: TokenKind<'s>> TokenIterator<'s, T> for Tokenizer<'s, T> {
    fn next(&mut self, logger: &mut Logger<'s>) -> Token<'s, T> {
        self.last_end = self.next.span().1.end;
        std::mem::replace(&mut self.next, Self::fetch_next(&mut self.cursor, logger))
    }
    fn peek(&self) -> &Token<'s, T> {
        &self.next
    }
    fn start(&self) -> usize {
        self.next.span().1.start
    }
    fn span_from(&self, start: usize) -> Span<'s> {
       Span(self.cursor.src(), start..self.last_end)
    }
    fn src(&self) -> &'s Src {
        self.cursor.src()
    }
}

#[derive(Debug)]
/// A list of pre-parsed tokens
pub struct TokenTree<'s, T: TokenKind<'s>> {
    tokens: std::vec::IntoIter<Token<'s, T>>,
    // Boxed because it's normal to include `TokenTree` in a `TokenKind`
    next: Box<Token<'s, T>>,
    last_end: usize,
    eof: (Option<String>, Span<'s>),
}

impl<'s, T: TokenKind<'s>> TokenTree<'s, T> {
    fn fetch_next(
        iter: &mut std::vec::IntoIter<Token<'s, T>>,
        (eof_name, eof_span): (Option<&str>, Span<'s>),
    ) -> Token<'s, T> {
        iter.next().unwrap_or_else(|| Token::new_eof(eof_name, eof_span.clone()))
    }
    pub fn new(tokens: Vec<Token<'s, T>>, eof: (Option<&str>, Span<'s>)) -> Self {
        let mut tokens = tokens.into_iter();
        Self {
            next: Box::from(Self::fetch_next(&mut tokens, eof.clone())),
            tokens,
            eof: (eof.0.map(|s| s.to_owned()), eof.1.clone()),
            last_end: 0,
        }
    }
    /// Consumes this tree and turns it into a vector of its contents
    pub fn into_items(self) -> Vec<Token<'s, T>> {
        self.tokens.collect()
    }
    pub fn parse_fully_into<N: NodeKind<'s, TokenKind = T>>(mut self, logger: &mut Logger<'s>) -> N {
        let res = N::parse(&mut self, logger);
        if !self.next.is_eof() {
            logger.log(Message::new(
                Level::Error,
                format!(
                    "expected {}, got {}",
                    self.eof.0.unwrap_or("end-of-file".to_string()),
                    self.next
                ),
                self.next.span(),
            ));
        }
        res
    }
}

impl<'s, T: TokenKind<'s>> TokenIterator<'s, T> for TokenTree<'s, T> {
    fn next(&mut self, _logger: &mut Logger<'s>) -> Token<'s, T> {
        self.last_end = self.next.span().1.end;
        std::mem::replace(
            &mut self.next,
            Self::fetch_next(&mut self.tokens, (self.eof.0.as_deref(), self.eof.1.clone()))
        )
    }
    fn peek(&self) -> &Token<'s, T> {
        &self.next
    }
    fn start(&self) -> usize {
        self.next.span().1.start
    }
    fn span_from(&self, start: usize) -> Span<'s> {
       Span(self.next.span().0, start..self.last_end)
    }
    fn src(&self) -> &'s Src {
        self.next.span().0
    }
}
