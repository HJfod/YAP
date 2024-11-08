use std::path::Path;
use strum::EnumString;
use prolangine::{
    log::Logger, parse::{
        common::{
            parse_c_like_num,
            parse_c_like_string,
            parse_c_like_word,
            parse_delimited,
            parse_matching,
            CommonDelimiters,
            Parenthesized
        },
        node::NodeKind,
        token::{ParsedTokenKind, Token, TokenKind, TokenTree}
    }, src::{Codebase, Span, Src, SrcCursor}
};

#[derive(Debug, PartialEq, strum::Display, EnumString)]
pub enum ExampleOp {
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "=")]
    Assign,
    #[strum(serialize = "==")]
    Eq,
}

fn is_op_char(ch: char) -> bool {
    matches!(ch, '+' | '-' | '=')
}

#[derive(Debug)]
pub enum ExampleLanguageToken<'s> {
    Number(f64),
    Ident(String),
    String(String),
    Op(ExampleOp),
    Parenthesis(TokenTree<'s, ExampleLanguageToken<'s>>),
}

impl<'s> TokenKind<'s> for ExampleLanguageToken<'s> {
    fn display_name(&self) -> String {
        match self {
            Self::Number(_) => String::from("number"),
            Self::Ident(_)  => String::from("identifier"),
            Self::String(_) => String::from("string"),
            Self::Op(_)     => String::from("operator"),
            Self::Parenthesis(_) => String::from("parenthesized expression"),
        }
    }

    fn next(cursor: &mut SrcCursor<'s>, logger: &mut Logger<'s>) -> Token<'s, ExampleLanguageToken<'s>> {
        let start = cursor.pos();

        // Check for EOF
        if cursor.peek().is_none() {
            return Token::new_eof(None, cursor.span_from(start));
        }

        // Identifiers & keywords
        if let Some(ident) = parse_c_like_word(cursor) {
            return Token::new(Self::Ident(ident.into()), cursor.span_from(start));
        }

        // Numbers
        if let Some((num, _)) = parse_c_like_num(cursor) {
            return match num.parse() {
                Ok(n) => Token::new(Self::Number(n), cursor.span_from(start)),
                Err(e) => {
                    logger.error(format!("invalid number {num} ({e})"), cursor.span_from(start));
                    Token::new(Self::Number(0.0), cursor.span_from(start))
                }
            }
        }

        // String literal
        if let Some(str) = parse_c_like_string(cursor, logger) {
            return Token::new(Self::String(str), cursor.span_from(start));
        }

        // Operators
        if let Some(op) = parse_matching(is_op_char, cursor) {
            return match ExampleOp::try_from(op) {
                Ok(op)   => Token::new(Self::Op(op), cursor.span_from(start)),
                Err(err) => {
                    logger.error(err.to_string(), cursor.span_from(start));
                    Token::new(Self::Op(ExampleOp::Plus), cursor.span_from(start))
                }
            };
        }

        // Parenthesis
        if let Some(tree) = parse_delimited("(", ")", cursor, logger) {
            return Token::new(Self::Parenthesis(tree), cursor.span_from(start));
        }

        logger.error(format!("invalid character '{}'", cursor.next().unwrap()), cursor.span_from(start));
        Token::new(ExampleLanguageToken::Op(ExampleOp::Assign), cursor.span_from(start))
    }
}

impl<'s> CommonDelimiters<'s> for ExampleLanguageToken<'s> {
    fn is_parenthesized(&self) -> bool {
        matches!(self, Self::Parenthesis(_))
    }
    fn parenthesized(self) -> Option<TokenTree<'s, Self>> {
        match self {
            Self::Parenthesis(p) => Some(p),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum AtomExpr<'s> {
    Closed(Parenthesized<'s, ExampleLanguageToken<'s>, Box<Expr<'s>>>),
    String(String, Span<'s>),
    Number(f64, Span<'s>),
}

impl<'s> NodeKind<'s> for AtomExpr<'s> {
    type TokenKind = ExampleLanguageToken<'s>;
    fn parse<I>(tokenizer: &mut I, logger: &mut prolangine::log::Logger<'s>) -> Self
        where
            Self: Sized,
            I: prolangine::parse::token::TokenIterator<'s, Self::TokenKind>
    {
        if Parenthesized::<'s, ExampleLanguageToken<'s>, Box<Expr<'s>>>::peek(tokenizer) {
            Self::Closed(Parenthesized::parse(tokenizer, logger))
        }
        else {
            let token = Token::<'s, ExampleLanguageToken<'s>>::parse(tokenizer, logger);
            match token.as_token() {
                Some(ExampleLanguageToken::String(n)) => Self::String(n.to_string(), token.span()),
                Some(ExampleLanguageToken::Number(n)) => Self::Number(*n, token.span()),
                _ => {
                    logger.expected("expression", &token, token.span());
                    Self::Number(0.0, token.span())
                }
            }
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: prolangine::parse::token::TokenIterator<'s, Self::TokenKind>
    {

        if Parenthesized::<'s, ExampleLanguageToken<'s>, Box<Expr<'s>>>::peek(tokenizer) {
            true
        }
        else {
            matches!(
                tokenizer.peek().as_token(),
                Some(ExampleLanguageToken::String(_) | ExampleLanguageToken::Number(_))
            )
        }
    }
    fn children(&self) -> Vec<&dyn NodeKind<'s, TokenKind = Self::TokenKind>> {
        match self {
            Self::Closed(e) => e.children(),
            Self::String(_, _) => vec![],
            Self::Number(_, _) => vec![],
        }
    }
    fn span(&self) -> Span<'s> {
        match self {
            Self::Closed(e) => e.span(),
            Self::String(_, s) => s.clone(),
            Self::Number(_, s) => s.clone(),
        }
    }
}

#[derive(Debug)]
pub enum Expr<'s> {
    Atom(AtomExpr<'s>),
}

impl<'s> NodeKind<'s> for Expr<'s> {
    type TokenKind = ExampleLanguageToken<'s>;
    fn parse<I>(tokenizer: &mut I, logger: &mut prolangine::log::Logger<'s>) -> Self
        where
            Self: Sized,
            I: prolangine::parse::token::TokenIterator<'s, Self::TokenKind>
    {
        if AtomExpr::peek(tokenizer) {
            Self::Atom(AtomExpr::parse(tokenizer, logger))
        }
        else {
            let token = Token::<'s, ExampleLanguageToken<'s>>::parse(tokenizer, logger);
            logger.expected("expression", &token, token.span());
            Self::Atom(AtomExpr::Number(0.0, token.span()))
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: prolangine::parse::token::TokenIterator<'s, Self::TokenKind>
    {
        AtomExpr::peek(tokenizer)
    }
    fn children(&self) -> Vec<&dyn NodeKind<'s, TokenKind = Self::TokenKind>> {
        match self {
            Self::Atom(e) => e.children(),
        }
    }
    fn span(&self) -> Span<'s> {
        match self {
            Self::Atom(e) => e.span(),
        }
    }
}

trait DebugEq {
    fn debug_eq(self, other: Self) -> Result<(), String>;
}

impl<'s> DebugEq for Token<'s, ExampleLanguageToken<'s>> {
    fn debug_eq(self, other: Token<'s, ExampleLanguageToken<'s>>) -> Result<(), String> {
        let self_str = format!("{:?}", self.kind());
        let other_str = format!("{:?}", other.kind());
        let matches = match (self.into_kind(), other.into_kind()) {
            (ParsedTokenKind::Token(a, _), ParsedTokenKind::Token(b, _)) => match (a, b) {
                (ExampleLanguageToken::Number(a), ExampleLanguageToken::Number(b)) => a == b,
                (ExampleLanguageToken::Ident(a), ExampleLanguageToken::Ident(b)) => a == b,
                (ExampleLanguageToken::String(a), ExampleLanguageToken::String(b)) => a == b,
                (ExampleLanguageToken::Op(a), ExampleLanguageToken::Op(b)) => a == b,
                (ExampleLanguageToken::Parenthesis(a), ExampleLanguageToken::Parenthesis(b)) => {
                    a.into_items().debug_eq(b.into_items())?;
                    true
                },
                (_, _) => false,
            },
            (ParsedTokenKind::EOF(_), ParsedTokenKind::EOF(_)) => true,
            (_, _) => false,
        };
        if matches {
            Ok(())
        }
        else {
            Err(format!("expected {}, got {}", other_str, self_str))
        }
    }
}
impl<'s> DebugEq for Vec<Token<'s, ExampleLanguageToken<'s>>> {
    fn debug_eq(self, other: Self) -> Result<(), String> {
        if self.len() != other.len() {
            return Err(format!(
                "expected token tree with {} items, got {} items",
                other.len(), self.len()
            ));
        }
        for (a, b) in self.into_iter().zip(other.into_iter()) {
            a.debug_eq(b)?;
        }
        Ok(())
    }
}

#[test]
fn parse_source_from_file() {
    let src = Src::from_file(Path::new("tests/src/test.example")).expect("Unable to read source");
    let mut logger = Logger::default();
    src.tokenize::<ExampleLanguageToken>(&mut logger);
    if logger.error_count() > 0 {
        panic!("tokenization produced errors");
    }
}

#[test]
#[should_panic]
fn parse_source_fail() {
    let src = Src::from_memory("test", r#"
        a = @
    "#);
    let mut logger = Logger::default();
    src.tokenize::<ExampleLanguageToken>(&mut logger);
    if logger.error_count() > 0 {
        panic!("tokenization produced errors");
    }
}

#[test]
fn parse_source_from_memory() {
    let src = Src::from_memory("test", r#"
        num = 2
        print(num + 4)
        print("hi everyone")
    "#);
    let mut logger = Logger::default();
    let tokens = src.tokenize::<ExampleLanguageToken>(&mut logger);
    let result = tokens.debug_eq(
        vec![
            ExampleLanguageToken::Ident("num".into()),
            ExampleLanguageToken::Op(ExampleOp::Assign),
            ExampleLanguageToken::Number(2.0),
            ExampleLanguageToken::Ident("print".into()),
            ExampleLanguageToken::Parenthesis(TokenTree::new(vec![
                ExampleLanguageToken::Ident("num".into()),
                ExampleLanguageToken::Op(ExampleOp::Plus),
                ExampleLanguageToken::Number(4.0),
            ].into_iter().map(|t| Token::new(t, src.span(0..0))).collect(), (Some(")"), src.span(0..0)))),
            ExampleLanguageToken::Ident("print".into()),
            ExampleLanguageToken::Parenthesis(TokenTree::new(vec![
                ExampleLanguageToken::String("hi everyone".into()),
            ].into_iter().map(|t| Token::new(t, src.span(0..0))).collect(), (Some(")"), src.span(0..0)))),
        ].into_iter().map(|t| Token::new(t, src.span(0..0))).collect()
    );
    assert!(result.is_ok(), "token stream mismatch: {}", result.unwrap_err());
}

#[test]
fn glob_source() {
    let codebase = Codebase::new_from_path(Path::new("tests/src"), &["example"]).expect("Unable to create codebase");
    for src in &codebase {
        let _tokens = src.tokenize::<ExampleLanguageToken>(&mut Logger::default());
    }
}
