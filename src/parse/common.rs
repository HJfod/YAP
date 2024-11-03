//! Common parsing utilities for crafting C-like languages

use std::marker::PhantomData;

use crate::{lang::Language, src::{Span, SrcCursor}};
use unicode_xid::UnicodeXID;

use super::{node::NodeKind, token::{TokenKind, TokenTree}};

/// Skip C-like comments (`// ...` line comments and `/* ... */` block comments)
/// in a source stream, as well as skipping whitespace
pub fn skip_c_like_comments<L: Language>(cursor: &mut SrcCursor<L>) {
    loop {
        // Ignore line comments
        if cursor.peek().is_some_and(|c| c == '/') && cursor.peek_n(1).is_some_and(|c| c == '/') {
            cursor.next();
            cursor.next();
            for c in &mut *cursor {
                if c == '\n' {
                    break;
                }
            }
            continue;
        }
        // Continue skipping until we encounter a non-whitespace character
        if cursor.peek().is_some_and(|c| c.is_whitespace()) {
            cursor.next();
            continue;
        }
        break;
    }
}

/// Parse a word that matches [XID_Start XID_Continue*], aka a keyword or an
/// identifier for most languages
pub fn parse_c_like_word<'s, L: Language>(cursor: &mut SrcCursor<'s, L>) -> Option<&'s str> {
    let start = cursor.pos();
    if cursor.next_if(UnicodeXID::is_xid_start).is_some() {
        while cursor.next_if(UnicodeXID::is_xid_continue).is_some() {}
        Some(cursor.span_from(start).data())
    }
    else {
        None
    }
}

/// Parses a C-like integer or decimal number. Note that this assumes decimal
/// numbers have digits on either side of the dot, so `.0` and `2.` are not
/// valid
pub fn parse_c_like_num<'s, L: Language>(cursor: &mut SrcCursor<'s, L>) -> Option<(&'s str, bool)> {
    let start = cursor.pos();
    if cursor.next_if(|c| c.is_ascii_digit()).is_some() {
        // Parse all digits found
        while cursor.next_if(|c| c.is_ascii_digit()).is_some() {}

        // Check for a dot followed by a number, aka meaning this is a decimal number
        let is_decimal = if cursor.peek() == Some('.')
            && cursor.peek_n(1).is_some_and(|c| char::is_ascii_digit(&c))
        {
            // Consume dot and first number
            cursor.next();
            cursor.next();

            // Consume rest of decimal
            while cursor.next_if(|c| c.is_ascii_digit()).is_some() {}

            true
        }
        else {
            false
        };
        Some((cursor.span_from(start).data(), is_decimal))
    }
    else {
        None
    }
}

pub struct ParseError<'s, L: Language> {
    pub msg: String,
    pub span: Span<'s, L>,
}

/// Parses a C-like string literal. Supports common escaping characters
pub fn parse_c_like_string<'s, L: Language>(
    cursor: &mut SrcCursor<'s, L>,
) -> Option<Result<String, ParseError<'s, L>>> {
    if cursor.next_if(|c| c == '"').is_some() {
        let mut escaped = String::new();
        loop {
            let Some(c) = cursor.next() else {
                return Some(Err(ParseError {
                    msg: "unterminated string literal".into(),
                    span: Span(cursor.src(), cursor.pos()..cursor.pos()),
                }));
            };
            if c == '"' {
                break;
            }
            escaped.push(match c {
                '\\' => match cursor.next() {
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('0') => '\0',
                    Some('r') => '\r',
                    Some('\\') => '\\',
                    Some('\"') => '\"',
                    Some('\'') => '\'',
                    Some(c) => {
                        return Some(Err(ParseError {
                            msg: format!("invalid escape sequence '\\{c}'"),
                            span: Span(cursor.src(), cursor.pos() - 1..cursor.pos()),
                        }));
                    }
                    None => {
                        return Some(Err(ParseError {
                            msg: "expected escape sequence".into(),
                            span: Span(cursor.src(), cursor.pos() - 1..cursor.pos()),
                        }));
                    }
                },
                o => o,
            });
        }
        Some(Ok(escaped))
    }
    else {
        None
    }
}

/// Expect an exact string to appear in the source string
pub fn parse_exact<'s, L: Language>(text: &str, cursor: &mut SrcCursor<'s, L>) -> Option<&'s str> {
    let start = cursor.pos();
    // First check that the entire string is coming up
    for (i, c) in text.chars().enumerate() {
        if cursor.peek_n(i) != Some(c) {
            return None;
        }
    }
    // Then consume that amount of characters
    // `iter::advance_by` isn't stable sadge :(
    for _ in text.chars() {
        cursor.next();
    }
    Some(cursor.span_from(start).data())
}

/// Parse characters matching a predicate until one that doesn't match is 
/// encountered
pub fn parse_matching<'s, L: Language, F>(matcher: F, cursor: &mut SrcCursor<'s, L>) -> Option<&'s str>
    where F: Fn(char) -> bool
{
    let start = cursor.pos();
    if cursor.next_if(&matcher).is_some() {
        while cursor.next_if(&matcher).is_some() {}
        Some(cursor.span_from(start).data())
    }
    else {
        None
    }
}

/// Parse a delimited sequence
pub fn parse_delimited<'s, L: Language>(
    open: &str,
    close: &str,
    cursor: &mut SrcCursor<'s, L>,
) -> Option<Result<TokenTree<'s, L>, ParseError<'s, L>>> {
    if parse_exact(open, cursor).is_some() {
        let mut tokens = Vec::new();
        let mut eof_start;
        loop {
            L::TokenKind::skip_to_next(cursor);

            eof_start = cursor.pos();

            // Closing delimiter
            if parse_exact(close, cursor).is_some() {
                break;
            }

            // Check for EOF (unclosed delimited sequence)
            let token = L::TokenKind::next(cursor);
            if token.is_eof() {
                return Some(Err(ParseError {
                    msg: format!("expected '{close}', got {}", token),
                    span: cursor.span_from(eof_start),
                }));
            }

            // Otherwise push this token and keep looking
            tokens.push(token);
        }
        Some(Ok(TokenTree::new(tokens, cursor.span_from(eof_start))))
    }
    else {
        None
    }
}

/// List of nodes separated by another node.
/// * **NOTE**: Use `SeparatedOptTrailing` if you want to allow trailing separators
/// * **NOTE**: the separator nodes are discarded and aren't actually stored!
#[derive(Debug)]
pub struct Separated<'s, L: Language, N: NodeKind<'s, L>, S: NodeKind<'s, L>> {
    items: Vec<N>,
    span: Span<'s, L>,
    _phantom: PhantomData<S>,
}

impl<'s, L: Language, N: NodeKind<'s, L>, S: NodeKind<'s, L>> NodeKind<'s, L> for Separated<'s, L, N, S> {
    fn parse<T>(tokenizer: &mut T) -> Self
        where
            Self: Sized,
            T: super::token::TokenIterator<'s, L>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            let mut items = vec![N::parse(tokenizer)];
            while S::peek(tokenizer) {
                S::parse(tokenizer);
                items.push(N::parse(tokenizer));
            }
            Self {
                items,
                span: tokenizer.span_from(start),
                _phantom: PhantomData,
            }
        }
        else {
            Self {
                items: vec![],
                span: Span(tokenizer.src(), start..start),
                _phantom: PhantomData,
            }
        }
    }
    fn peek<T>(tokenizer: &T) -> bool
        where
            Self: Sized,
            T: super::token::TokenIterator<'s, L>
    {
        N::peek(tokenizer)
    }
    fn children(&self) -> Vec<&dyn NodeKind<'s, L>> {
        self.items.iter().map(|i| i as &dyn NodeKind<'s, L>).collect()
    }
    fn span(&self) -> Span<'s, L> {
        self.span.clone()
    }
}

/// List of nodes separated by another node with optional trailing separator
/// * **NOTE**: the separator nodes are discarded and aren't actually stored!
#[derive(Debug)]
pub struct SeparatedOptTrailing<'s, L: Language, N: NodeKind<'s, L>, S: NodeKind<'s, L>> {
    items: Vec<N>,
    span: Span<'s, L>,
    _phantom: PhantomData<S>,
}

impl<'s, L: Language, N: NodeKind<'s, L>, S: NodeKind<'s, L>> NodeKind<'s, L> for SeparatedOptTrailing<'s, L, N, S> {
    fn parse<T>(tokenizer: &mut T) -> Self
        where
            Self: Sized,
            T: super::token::TokenIterator<'s, L>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            let mut items = vec![N::parse(tokenizer)];
            while S::peek(tokenizer) {
                S::parse(tokenizer);
                if N::peek(tokenizer) {
                    items.push(N::parse(tokenizer));
                }
                else {
                    break;
                }
            }
            Self {
                items,
                span: tokenizer.span_from(start),
                _phantom: PhantomData,
            }
        }
        else {
            Self {
                items: vec![],
                span: Span(tokenizer.src(), start..start),
                _phantom: PhantomData,
            }
        }
    }
    fn peek<T>(tokenizer: &T) -> bool
        where
            Self: Sized,
            T: super::token::TokenIterator<'s, L>
    {
        N::peek(tokenizer)
    }
    fn children(&self) -> Vec<&dyn NodeKind<'s, L>> {
        self.items.iter().map(|i| i as &dyn NodeKind<'s, L>).collect()
    }
    fn span(&self) -> Span<'s, L> {
        self.span.clone()
    }
}
