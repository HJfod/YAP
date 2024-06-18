use std::path::Path;
use strum::EnumString;
use yap::{
    lang::Language,
    parse::{
        common::{parse_c_like_num, parse_c_like_string, parse_c_like_word, parse_delimited, parse_matching},
        token::{parse_fully_into_tokens, Token, TokenKind, TokenTree},
    },
    src::Src,
};

pub struct ExampleLanguage;

impl Language for ExampleLanguage {
    fn file_extensions() -> &'static [&'static str] {
        &["example"]
    }
}

#[derive(Debug, EnumString)]
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
    Parenthesis(TokenTree<'s, Self>),
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

    fn next(cursor: &mut yap::src::SrcCursor<'s>) -> Token<'s, Self> {
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
        if let Some(res) = parse_delimited::<Self>("(", ")", cursor) {
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

#[test]
fn parse_source_from_file() {
    let src = Src::from_file(Path::new("tests/src/test.example")).expect("Unable to read source");
    let tokens = parse_fully_into_tokens::<ExampleLanguageToken>(&src);
    for token in tokens {
        if token.is_error() {
            panic!("error token encountered: {token:?}");
        }
    }
}
