use std::path::Path;
// use prolangine_macros::NodeKind;
use strum::EnumString;
use prolangine::{
    parse::{
        common::{parse_c_like_num, parse_c_like_string, parse_c_like_word, parse_delimited, parse_matching, CommonDelimiters, Parenthesized}, node::NodeKind, token::{ParsedTokenKind, Token, TokenKind, TokenTree}
    },
    src::{Codebase, Span, Src, SrcCursor},
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

    fn next(cursor: &mut SrcCursor<'s>) -> Token<'s, ExampleLanguageToken<'s>> {
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
            return num
                .parse()
                .map(|n| Token::new(Self::Number(n), cursor.span_from(start)))
                .unwrap_or(Token::new_error(
                    &format!("invalid number {num}"),
                    cursor.span_from(start),
                ));
        }

        // String literal
        if let Some(res) = parse_c_like_string(cursor) {
            return match res {
                Ok(str)  => Token::new(Self::String(str), cursor.span_from(start)),
                Err(err) => Token::new_error(&err.msg, err.span),
            };
        }

        // Operators
        if let Some(op) = parse_matching(is_op_char, cursor) {
            return match ExampleOp::try_from(op) {
                Ok(op)   => Token::new(Self::Op(op), cursor.span_from(start)),
                Err(err) => Token::new_error(&err.to_string(), cursor.span_from(start)),
            };
        }

        // Parenthesis
        if let Some(res) = parse_delimited("(", ")", cursor) {
            return match res {
                Ok(tree) => Token::new(Self::Parenthesis(tree), cursor.span_from(start)),
                Err(err) => Token::new_error(&err.msg, err.span),
            };
        }

        Token::new_error(
            &format!("invalid character '{}'", cursor.next().unwrap()),
            cursor.span_from(start),
        )
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

// #[derive(Debug)]
// pub enum AtomExpr<'s> {
//     Closed(Parenthesized<'s, ExampleLanguageToken<'s>, Box<Expr<'s>>>),
//     String(String, Span<'s>),
//     Number(f64, Span<'s>),
// }

// #[derive(Debug, NodeKind)]
// #[parse(expected = "expression")]
// pub enum Expr<'s> {
//     Atom(AtomExpr<'s>),
// }

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
            (ParsedTokenKind::Error(_), ParsedTokenKind::Error(_)) => true,
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
    let tokens = src.tokenize::<ExampleLanguageToken>();
    for token in tokens {
        if token.is_error() {
            panic!("error token encountered: {token:?}");
        }
    }
}

#[test]
fn parse_source_from_memory() {
    let src = Src::from_memory("test", r#"
        num = 2
        print(num + 4)
        print("hi everyone")
    "#);
    let tokens = src.tokenize::<ExampleLanguageToken>();
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
            ].into_iter().map(|t| Token::new(t, Span::builtin())).collect(), (Some(")"), Span::builtin()))),
            ExampleLanguageToken::Ident("print".into()),
            ExampleLanguageToken::Parenthesis(TokenTree::new(vec![
                ExampleLanguageToken::String("hi everyone".into()),
            ].into_iter().map(|t| Token::new(t, Span::builtin())).collect(), (Some(")"), Span::builtin()))),
        ].into_iter().map(|t| Token::new(t, Span::builtin())).collect()
    );
    assert!(result.is_ok(), "token stream mismatch: {}", result.unwrap_err());
}

#[test]
fn glob_source() {
    let codebase = Codebase::new_from_path(Path::new("tests/src"), &["example"]).expect("Unable to create codebase");
    for src in &codebase {
        let _tokens = src.tokenize::<ExampleLanguageToken>();
    }
}
