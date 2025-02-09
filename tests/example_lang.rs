use std::path::Path;
use prolangine_macros::{create_token_nodes, NodeKind};
use prolangine::{
    parse::{
        common::{
            parse_c_like_num,
            parse_c_like_string,
            parse_c_like_word,
            parse_delimited,
            parse_matching,
            ParsedString
        },
        node::{Node, NodeKind},
        token::{DisplayName, ParsedTokenKind, Token, TokenKind, TokenTree}
    },
    src::{Codebase, Span, SrcCursor}
};

fn is_op_char(ch: char) -> bool {
    matches!(ch, '+' | '-' | '=')
}

#[create_token_nodes]
pub enum ExampleLanguageToken {
    Number(f64),
    String(String),

    #[token(display_name = "identifier")]
    Ident(String),

    #[token(display_name = "'+'")]
    Plus,
    #[token(display_name = "'-'")]
    Minus,
    #[token(display_name = "'='")]
    Assign,
    #[token(display_name = "'=='")]
    Eq,

    #[token(node = "Parenthesized<T>")]
    Parenthesis(TokenTree<ExampleLanguageToken>),
}

impl TokenKind for ExampleLanguageToken {
    fn next(cursor: &mut SrcCursor) -> Token<ExampleLanguageToken> {
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
                Err(e) => Token::new_error(format!("invalid number {num} ({e})"), cursor.span_from(start)),
            }
        }

        // String literal
        if let Some(str) = parse_c_like_string(cursor) {
            return match str {
                ParsedString::Parsed(str) => Token::new(Self::String(str), cursor.span_from(start)),
                ParsedString::InvalidEscapeSequences(seqs) => {
                    // TODO: tokens can't contain multiple errors, so either
                    // a) Make that possible
                    // b) Make InvalidString its own token type and carry the information along
                    //    (bad because then `StringToken::parse()` doesn't always work)
                    // c) Include the errors in the String token
                    // d) Just deal with this
                    let (c, span) = seqs.into_iter().next().unwrap();
                    Token::new_error(format!("invalid escape sequence '\\{c}'"), span)
                }
                ParsedString::Unterminated(span) => Token::new_error("unterminated string literal", span),
            }
        }

        // Operators
        if let Some(op) = parse_matching(is_op_char, cursor) {
            return match op {
                "==" => Token::new(Self::Eq, cursor.span_from(start)),
                "+" => Token::new(Self::Plus, cursor.span_from(start)),
                "-" => Token::new(Self::Minus, cursor.span_from(start)),
                "=" => Token::new(Self::Assign, cursor.span_from(start)),
                o => Token::new_error(format!("invalid operator '{o}'"), cursor.span_from(start)),
            }
        }

        // Parenthesis
        if let Some(tree) = parse_delimited("(", ")", cursor) {
            return Token::new(Self::Parenthesis(tree), cursor.span_from(start));
        }

        Token::new_error(
            format!("invalid character '{}'", cursor.next().unwrap()),
            cursor.span_from(start)
        )
    }
}

#[derive(Debug, NodeKind)]
#[parse(expected = "expression", token_type = "ExampleLanguageToken")]
pub enum AtomExpr {
    Closed(Node<Parenthesized<Expr>>),
    String(Node<StringToken>),
    Number(Node<NumberToken>),
    Ident(Node<IdentToken>),
}

#[derive(Debug, NodeKind)]
#[parse(expected = "expression", token_type = "ExampleLanguageToken")]
pub enum Expr {
    Atom(Node<AtomExpr>),
}

trait DebugEq {
    fn debug_eq(self, other: Self) -> Result<(), String>;
}

impl DebugEq for Token<ExampleLanguageToken> {
    fn debug_eq(self, other: Token<ExampleLanguageToken>) -> Result<(), String> {
        let self_str = format!("{:?}", self.kind());
        let other_str = format!("{:?}", other.kind());
        let matches = match (self.into_kind(), other.into_kind()) {
            (ParsedTokenKind::Token(a), ParsedTokenKind::Token(b)) => match (a, b) {
                (ExampleLanguageToken::Number(a), ExampleLanguageToken::Number(b)) => a == b,
                (ExampleLanguageToken::Ident(a), ExampleLanguageToken::Ident(b)) => a == b,
                (ExampleLanguageToken::String(a), ExampleLanguageToken::String(b)) => a == b,
                (ExampleLanguageToken::Plus, ExampleLanguageToken::Plus) => true,
                (ExampleLanguageToken::Minus, ExampleLanguageToken::Minus) => true,
                (ExampleLanguageToken::Assign, ExampleLanguageToken::Assign) => true,
                (ExampleLanguageToken::Eq, ExampleLanguageToken::Eq) => true,
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
impl DebugEq for Vec<Token<ExampleLanguageToken>> {
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
    let mut codebase = Codebase::new(Path::new("tests/src"));
    let src = codebase.add_src(Path::new("tests/src/test.example")).expect("Unable to read source");
    if src.tokenize::<ExampleLanguageToken>().into_iter().any(|t| t.is_error()) {
        panic!("tokenization produced errors");
    }
    let node: Node<Expr> = src.parse();
    if node.contains_errors() {
        panic!("ran into errors: {:?}", node.get_errors());
    }
}

#[test]
fn test_token_display_names() {
    assert_eq!(ExampleLanguageToken::Ident(String::new()).display_name().as_str(), "identifier");
}

#[test]
#[should_panic]
fn parse_source_fail() {
    let mut codebase = Codebase::new(Path::new("tests/src"));
    let src = codebase.add_src_from_memory("test", r#"
        a = @
    "#);
    if src.tokenize::<ExampleLanguageToken>().into_iter().any(|t| t.is_error()) {
        panic!("tokenization produced errors");
    }
}

#[test]
fn parse_source_from_memory() {
    let mut codebase = Codebase::new(Path::new("tests/src"));
    let src = codebase.add_src_from_memory("test", r#"
        num = 2
        print(num + 4)
        print("hi everyone")
    "#);
    let tokens = src.tokenize::<ExampleLanguageToken>();
    let result = tokens.debug_eq(
        vec![
            ExampleLanguageToken::Ident("num".into()),
            ExampleLanguageToken::Assign,
            ExampleLanguageToken::Number(2.0),
            ExampleLanguageToken::Ident("print".into()),
            ExampleLanguageToken::Parenthesis(TokenTree::new(vec![
                ExampleLanguageToken::Ident("num".into()),
                ExampleLanguageToken::Plus,
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
    let mut codebase = Codebase::new(Path::new("tests/src"));
    codebase.add_src_recursive(Path::new("tests/src"), &["example"]).expect("Unable to create codebase");
    for src in &codebase {
        let _tokens = src.tokenize::<ExampleLanguageToken>();
    }
}
