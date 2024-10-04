use crate::lang::Language;
use crate::src::{Span, Src, SrcCursor};
use std::fmt::Debug;
use std::fmt::Display;

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
pub trait TokenKind<'s, L: Language>: Debug + Sized {
    /// Get the display name for this token
    fn display_name(&self) -> String;

    /// Get the next token in the source iterator. This method is **not required**
    /// to run [`TokenKind::skip_to_next`]; it may assume that the next token
    /// starts where the cursor is currently pointing at
    fn next(cursor: &mut SrcCursor<'s, L>) -> Token<'s, L>;

    /// Skip to the next token; by default skips whitespace and C-like comments
    /// (`// line comment` and `/* block comment */`)
    fn skip_to_next(cursor: &mut SrcCursor<'s, L>) {
        skip_c_like_comments(cursor);
    }
}

#[derive(Debug)]
pub enum ParsedTokenKind<'s, L: Language> {
    Token(L::TokenKind<'s>),
    EOF(Option<String>),
    Error(String),
}

#[derive(Debug)]
pub struct Token<'s, L: Language> {
    kind: ParsedTokenKind<'s, L>,
    span: Span<'s, L>,
}

impl<'s, L: Language> Token<'s, L> {
    pub fn new(kind: L::TokenKind<'s>, span: Span<'s, L>) -> Self {
        Self {
            kind: ParsedTokenKind::Token(kind),
            span,
        }
    }
    pub fn new_eof(name: Option<&str>, span: Span<'s, L>) -> Self {
        Self {
            kind: ParsedTokenKind::EOF(name.map(|n| n.into())),
            span,
        }
    }
    pub fn new_error(msg: &str, span: Span<'s, L>) -> Self {
        Self {
            kind: ParsedTokenKind::Error(msg.into()),
            span,
        }
    }

    pub fn kind(&self) -> &ParsedTokenKind<'s, L> {
        &self.kind
    }
    pub fn into_kind(self) -> ParsedTokenKind<'s, L> {
        self.kind
    }
    pub fn span(&self) -> Span<'s, L> {
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

impl<'s, L: Language> Display for Token<'s, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParsedTokenKind::Token(t) => f.write_str(&t.display_name()),
            ParsedTokenKind::EOF(name) => {
                f.write_str(name.as_ref().map(|n| n.as_str()).unwrap_or("end-of-file"))
            }
            ParsedTokenKind::Error(error) => f.write_str(error),
        }
    }
}

/// Anything that can return Tokens should impl TokenIterator; which is not an
/// Iterator because it can never return None
pub(crate) trait TokenIterator<'s, L: Language> {
    fn next(&mut self) -> Token<'s, L>;
}

/// Turns an unparsed source file into tokens
pub(crate) struct Tokenizer<'s, L: Language> {
    cursor: SrcCursor<'s, L>,
}
impl<'s, L: Language> Tokenizer<'s, L> {
    pub fn new(src: &'s Src<L>) -> Self {
        Self {
            cursor: src.cursor(),
        }
    }
}
impl<'s, L: Language> TokenIterator<'s, L> for Tokenizer<'s, L> {
    fn next(&mut self) -> Token<'s, L> {
        L::TokenKind::skip_to_next(&mut self.cursor);
        L::TokenKind::next(&mut self.cursor)
    }
}

#[derive(Debug)]
/// A list of pre-parsed tokens
pub struct TokenTree<'s, L: Language> {
    tokens: std::vec::IntoIter<Token<'s, L>>,
    eof_span: Span<'s, L>,
}

impl<'s, L: Language> TokenTree<'s, L> {
    pub fn new(tokens: Vec<Token<'s, L>>, eof_span: Span<'s, L>) -> Self {
        Self {
            tokens: tokens.into_iter(),
            eof_span,
        }
    }
    /// Consumes this tree and turns it into a vector of its contents
    pub fn into_items(self) -> Vec<Token<'s, L>> {
        self.tokens.collect()
    }
}

impl<'s, L: Language> TokenIterator<'s, L> for TokenTree<'s, L> {
    fn next(&mut self) -> Token<'s, L> {
        self.tokens
            .next()
            .unwrap_or(Token::new_eof(None, self.eof_span.clone()))
    }
}
