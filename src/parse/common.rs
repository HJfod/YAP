//! Common parsing utilities for crafting C-like languages

use std::marker::PhantomData;

use crate::src::{Span, SrcCursor};
use unicode_xid::UnicodeXID;

use super::{node::{NodeKind, Parse}, token::{Error, ParseResult, TokenIterator, TokenKind, TokenTree}};

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

pub enum ParsedString {
    Parsed(String),
    InvalidEscapeSequences(Vec<(char, Span)>),
    Unterminated(Span),
}

/// Parses a C-like string literal. Supports common escaping characters
pub fn parse_c_like_string(cursor: &mut SrcCursor) -> Option<ParsedString> {
    // todo: smart multiline string literal
    if cursor.next_if(|c| c == '"').is_some() {
        let mut escaped = String::new();
        let mut invalid_sequences = vec![];
        loop {
            let Some(c) = cursor.next() else {
                return Some(ParsedString::Unterminated(cursor.src().span(cursor.pos()..cursor.pos())));
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
                        invalid_sequences.push((c, cursor.src().span(cursor.pos() - 1..cursor.pos())));
                        c
                    }
                    None => {
                        return Some(ParsedString::Unterminated(cursor.src().span(cursor.pos() - 1..cursor.pos())));
                    }
                },
                o => o,
            });
        }
        Some(if !invalid_sequences.is_empty() {
            ParsedString::InvalidEscapeSequences(invalid_sequences)
        }
        else {
            ParsedString::Parsed(escaped)
        })
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
) -> Option<ParseResult<TokenTree<T>>> {
    if parse_exact(open, cursor).is_some() {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut eof_start;
        loop {
            T::skip_to_next(cursor);

            eof_start = cursor.pos();

            // Closing delimiter
            if parse_exact(close, cursor).is_some() {
                break;
            }

            // Check for EOF (unclosed delimited sequence)
            match T::next(cursor) {
                Ok(token) => {
                    if token.is_eof() {
                        errors.push(Error::expected(close, &token, token.span()));
                        break;
                    }
                    // Otherwise push this token and keep looking
                    tokens.push(token);
                }
                Err(e) => errors.extend(e),
            }
        }
        if !errors.is_empty() {
            return Some(Err(errors));
        }
        Some(Ok(TokenTree::new(tokens, (Some(&format!("'{}'", close)), cursor.span_from(eof_start)))))
    }
    else {
        None
    }
}

pub fn try_parse_discard<T: TokenKind, N: Parse<T>>(tokenizer: &mut impl TokenIterator<T>, errors: &mut Vec<Error>) {
    match N::parse(tokenizer) {
        Ok(_) => {}
        Err(e) => errors.extend(e),
    }
}
pub fn try_parse_into_list<T: TokenKind, N: Parse<T>>(
    tokenizer: &mut impl TokenIterator<T>, list: &mut Vec<N>, errors: &mut Vec<Error>
) {
    match N::parse(tokenizer) {
        Ok(n) => if errors.is_empty() { list.push(n) }
        Err(e) => errors.extend(e),
    }
}

#[derive(Debug)]
pub enum Maybe<N: NodeKind> {
    Some(N),
    None(Span),
}

impl<T: TokenKind, N: NodeKind + Parse<T>> Parse<T> for Maybe<N> {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            Ok(Self::Some(N::parse(tokenizer)?))
        }
        else {
            Ok(Self::None(tokenizer.span_from(start)))
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

/// Zero or more of specific type of node. Wants to parse the entire source 
/// stream!
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
}
impl<T: TokenKind, N: NodeKind + Parse<T>> Parse<T> for ZeroOrMore<N> {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        let mut items = vec![];
        let mut errors = vec![];
        while !tokenizer.is_eof() {
            try_parse_into_list(tokenizer, &mut items, &mut errors);
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(Self {
            items,
            span: tokenizer.span_from(start),
        })
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        N::peek(tokenizer)
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

impl<N: NodeKind, S: NodeKind> NodeKind for Separated<N, S> {
    fn children(&self) -> Vec<&dyn NodeKind> {
        self.items.iter().map(|n| n as &dyn NodeKind).collect()
    }
    fn span(&self) -> Span {
        self.span.clone()
    }
}
impl<
    T: TokenKind,
    N: NodeKind + Parse<T>,
    S: NodeKind + Parse<T>
> Parse<T> for Separated<N, S> {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            let mut items = vec![];
            let mut errors = vec![];
            try_parse_into_list(tokenizer, &mut items, &mut errors);
            while !tokenizer.is_eof() {
                try_parse_discard::<T, S>(tokenizer, &mut errors);
                try_parse_into_list(tokenizer, &mut items, &mut errors);
            }
            if !errors.is_empty() {
                return Err(errors);
            }
            Ok(Self {
                items,
                span: tokenizer.span_from(start),
                _phantom: PhantomData,
            })
        }
        else {
            Ok(Self {
                items: vec![],
                span: tokenizer.src().span(start..start),
                _phantom: PhantomData,
            })
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

/// List of nodes separated by another node with optional trailing separator
/// * **NOTE**: the separator nodes are discarded and aren't actually stored!
#[derive(Debug)]
pub struct SeparatedOptTrailing<N: NodeKind, S: NodeKind> {
    items: Vec<N>,
    span: Span,
    _phantom: PhantomData<S>,
}

impl<N: NodeKind, S: NodeKind> NodeKind for SeparatedOptTrailing<N, S> {
    fn children(&self) -> Vec<&dyn NodeKind> {
        self.items.iter().map(|n| n as &dyn NodeKind).collect()
    }
    fn span(&self) -> Span {
        self.span.clone()
    }
}
impl<
    T: TokenKind,
    N: NodeKind + Parse<T>,
    S: NodeKind + Parse<T>
> Parse<T> for SeparatedOptTrailing<N, S> {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
        where
            Self: Sized,
            I: TokenIterator<T>
    {
        let start = tokenizer.start();
        if N::peek(tokenizer) {
            let mut items = vec![];
            let mut errors = vec![];
            try_parse_into_list(tokenizer, &mut items, &mut errors);
            while !tokenizer.is_eof() {
                try_parse_discard::<T, S>(tokenizer, &mut errors);
                if !tokenizer.is_eof() {
                    try_parse_into_list(tokenizer, &mut items, &mut errors);
                }
            }
            if !errors.is_empty() {
                return Err(errors);
            }
            Ok(Self {
                items,
                span: tokenizer.span_from(start),
                _phantom: PhantomData,
            })
        }
        else {
            Ok(Self {
                items: vec![],
                span: tokenizer.src().span(start..start),
                _phantom: PhantomData,
            })
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
