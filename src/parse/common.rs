//! Common parsing utilities for crafting C-like languages

use std::marker::PhantomData;

use crate::{log::{Level, Logger, Message}, src::{Span, SrcCursor}};
use unicode_xid::UnicodeXID;

use super::{node::NodeKind, token::{TokenIterator, TokenKind, TokenTree}};

pub trait CommonDelimiters: TokenKind {
    fn is_parenthesized(&self) -> bool {
        false
    }
    fn parenthesized(self) -> Option<TokenTree<Self>> {
        None
    }
    fn is_bracketed(&self) -> bool {
        false
    }
    fn bracketed(self) -> Option<TokenTree<Self>> {
        None
    }
    fn is_braced(&self) -> bool {
        false
    }
    fn braced(self) -> Option<TokenTree<Self>> {
        None
    }
    fn is_angle_bracketed(&self) -> bool {
        false
    }
    fn angle_bracketed(self) -> Option<TokenTree<Self>> {
        None
    }
}

/// Skip C-like comments (`// ...` line comments and `/* ... */` block comments)
/// in a source stream, as well as skipping whitespace
pub fn skip_c_like_comments(cursor: &mut SrcCursor) {
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
pub fn parse_c_like_word<'s>(cursor: &mut SrcCursor<'s>) -> Option<&'s str> {
    let start = cursor.pos();
    if cursor.next_if(UnicodeXID::is_xid_start).is_some() {
        while cursor.next_if(UnicodeXID::is_xid_continue).is_some() {}
        Some(cursor.data_from(start))
    }
    else {
        None
    }
}

/// Parses a C-like integer or decimal number. Note that this assumes decimal
/// numbers have digits on either side of the dot, so `.0` and `2.` are not
/// valid
pub fn parse_c_like_num<'s>(cursor: &mut SrcCursor<'s>) -> Option<(&'s str, bool)> {
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
        Some((cursor.data_from(start), is_decimal))
    }
    else {
        None
    }
}

/// Parses a C-like string literal. Supports common escaping characters
pub fn parse_c_like_string(cursor: &mut SrcCursor, logger: &mut Logger) -> Option<String> {
    // todo: smart multiline string literal
    if cursor.next_if(|c| c == '"').is_some() {
        let mut escaped = String::new();
        loop {
            let Some(c) = cursor.next() else {
                logger.error("unterminated string literal", cursor.src().span(cursor.pos()..cursor.pos()));
                break;
            };
            if c == '"' {
                break;
            }
            if let Some(c) = match c {
                '\\' => match cursor.next() {
                    Some('n') => Some('\n'),
                    Some('t') => Some('\t'),
                    Some('0') => Some('\0'),
                    Some('r') => Some('\r'),
                    Some('\\') => Some('\\'),
                    Some('\"') => Some('\"'),
                    Some('\'') => Some('\''),
                    Some(c) => {
                        logger.error(
                            format!("invalid escape sequence '\\{c}'"),
                            cursor.src().span(cursor.pos() - 1..cursor.pos()),
                        );
                        None
                    }
                    None => {
                        logger.error(
                            "expected escape sequence",
                            cursor.src().span(cursor.pos() - 1..cursor.pos()),
                        );
                        None
                    }
                },
                o => Some(o),
            } {
                escaped.push(c);
            }
        }
        Some(escaped)
    }
    else {
        None
    }
}

/// Expect an exact string to appear in the source string
pub fn parse_exact<'s>(text: &str, cursor: &mut SrcCursor<'s>) -> Option<&'s str> {
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
    Some(cursor.data_from(start))
}

/// Parse characters matching a predicate until one that doesn't match is 
/// encountered
pub fn parse_matching<'s, F>(matcher: F, cursor: &mut SrcCursor<'s>) -> Option<&'s str>
    where F: Fn(char) -> bool
{
    let start = cursor.pos();
    if cursor.next_if(&matcher).is_some() {
        while cursor.next_if(&matcher).is_some() {}
        Some(cursor.data_from(start))
    }
    else {
        None
    }
}

/// Parse a delimited sequence
pub fn parse_delimited<T: TokenKind>(
    open: &str,
    close: &str,
    cursor: &mut SrcCursor,
    logger: &mut Logger,
) -> Option<TokenTree<T>> {
    if parse_exact(open, cursor).is_some() {
        let mut tokens = Vec::new();
        let mut eof_start;
        loop {
            T::skip_to_next(cursor);

            eof_start = cursor.pos();

            // Closing delimiter
            if parse_exact(close, cursor).is_some() {
                break;
            }

            // Check for EOF (unclosed delimited sequence)
            let token = T::next(cursor, logger);
            if token.is_eof() {
                logger.expected(close, &token, token.span());
                break;
            }

            // Otherwise push this token and keep looking
            tokens.push(token);
        }
        Some(TokenTree::new(tokens, (Some(close), cursor.span_from(eof_start))))
    }
    else {
        None
    }
}

/// List of nodes separated by another node.
/// * **NOTE**: Use `SeparatedOptTrailing` if you want to allow trailing separators
/// * **NOTE**: the separator nodes are discarded and aren't actually stored!
#[derive(Debug)]
pub struct Separated<N: NodeKind, S: NodeKind> {
    items: Vec<N>,
    span: Span,
    _phantom: PhantomData<S>,
}

impl<
    T: TokenKind,
    N: NodeKind<TokenKind = T>,
    S: NodeKind<TokenKind = T>
> NodeKind for Separated<N, S> {
    type TokenKind = T;
    fn parse<I>(tokenizer: &mut I, logger: &mut Logger) -> Self
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            let mut items = vec![N::parse(tokenizer, logger)];
            while S::peek(tokenizer) {
                S::parse(tokenizer, logger);
                items.push(N::parse(tokenizer, logger));
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
                span: tokenizer.src().span(start..start),
                _phantom: PhantomData,
            }
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        N::peek(tokenizer)
    }
    fn children(&self) -> Vec<&dyn NodeKind<TokenKind = T>> {
        self.items.iter()
            .map(|n| n as &dyn NodeKind<TokenKind = T>)
            .collect()
    }
    fn span(&self) -> Span {
        self.span.clone()
    }
}

/// List of nodes separated by another node with optional trailing separator
/// * **NOTE**: the separator nodes are discarded and aren't actually stored!
#[derive(Debug)]
pub struct SeparatedOptTrailing<N: NodeKind, S: NodeKind> {
    items: Vec<N>,
    span: Span,
    _phantom: PhantomData<S>,
}

impl<
    T: TokenKind,
    N: NodeKind<TokenKind = T>,
    S: NodeKind<TokenKind = T>
> NodeKind for SeparatedOptTrailing<N, S> {
    type TokenKind = T;
    fn parse<I>(tokenizer: &mut I, logger: &mut Logger) -> Self
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            let mut items = vec![N::parse(tokenizer, logger)];
            while S::peek(tokenizer) {
                S::parse(tokenizer, logger);
                if N::peek(tokenizer) {
                    items.push(N::parse(tokenizer, logger));
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
                span: tokenizer.src().span(start..start),
                _phantom: PhantomData,
            }
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        N::peek(tokenizer)
    }
    fn children(&self) -> Vec<&dyn NodeKind<TokenKind = T>> {
        self.items.iter()
            .map(|n| n as &dyn NodeKind<TokenKind = T>)
            .collect()
    }
    fn span(&self) -> Span {
        self.span.clone()
    }
}

macro_rules! def_delimited_node {
    ($name: ident, $is_func: ident, $as_func: ident, $str: literal) => {
        #[derive(Debug)]
        pub struct $name<T: CommonDelimiters, N: NodeKind<TokenKind = T>> {
            item: Option<N>,
            span: Span,
            _phantom: PhantomData<T>,
        }

        impl<T: CommonDelimiters, N: NodeKind<TokenKind = T>> NodeKind for $name<T, N> {
            type TokenKind = T;
            fn parse<I>(tokenizer: &mut I, logger: &mut Logger) -> Self
                where
                    Self: Sized,
                    I: TokenIterator<Self::TokenKind>
            {
                let start = tokenizer.start();
                let token = tokenizer.next(logger);
                if token.as_token().is_some_and(|t| t.$is_func()) {
                    Self {
                        item: Some(
                            token.into_token().unwrap()
                                .$as_func().unwrap()
                                .parse_fully_into::<N>(logger)
                        ),
                        span: tokenizer.span_from(start),
                        _phantom: PhantomData,
                    }
                }
                else {
                    logger.log(Message::new(
                        Level::Error,
                        format!(concat!("expected ", $str, ", got {}"), token),
                        tokenizer.span_from(start),
                    ));
                    Self {
                        item: None,
                        span: tokenizer.span_from(start),
                        _phantom: PhantomData,
                    }
                }
            }
            fn peek<I>(tokenizer: &I) -> bool
                where
                    Self: Sized,
                    I: TokenIterator<Self::TokenKind>
            {
                tokenizer.peek().as_token().is_some_and(|t| t.$is_func())
            }
            fn children(&self) -> Vec<&dyn NodeKind<TokenKind = Self::TokenKind>> {
                self.item.as_ref().map(|n| n.children()).unwrap_or_default()
            }
            fn span(&self) -> Span {
                self.span.clone()
            }
        }
    };
}

def_delimited_node!(Parenthesized, is_parenthesized, parenthesized, "parentheses");
def_delimited_node!(Bracketed, is_bracketed, bracketed, "brackets");
def_delimited_node!(Braced, is_braced, braced, "braces");
def_delimited_node!(AngleBracketed, is_angle_bracketed, angle_bracketed, "angle brackets");
