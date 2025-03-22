use std::path::Path;
use prolangine_macros::{create_token_nodes, Fabricate, NodeKind, Parse};
use prolangine::{
    parse::{
        common::{
            parse_c_like_num,
            parse_c_like_string,
            parse_c_like_word,
            parse_delimited,
            parse_exact,
            parse_matching,
            ParsedString,
            SeparatedOptTrailing,
        }, node::{NodeKind, Parse}, token::{DisplayName, Error, Fabricate, ParseResult, ParsedTokenKind, Token, TokenIterator, TokenKind, TokenTree}
    },
    src::{Codebase, Span, SrcCursor}
};

fn is_op_char(ch: char) -> bool {
    matches!(ch, '+' | '-' | '=')
}

#[create_token_nodes]
#[derive(Fabricate)]
pub enum ExampleLanguageToken {
    Number(f64),
    String(String),

    #[token(display_name = "identifier")]
    Ident(String),

    #[token(display_name = "'+'")]
    Plus,
    #[token(display_name = "'-'")]
    Minus,
    #[token(display_name = "=")]
    Assign,
    #[token(display_name = "==")]
    Eq,

    #[token(display_name = "semicolon")]
    Semicolon,
    #[token(display_name = "comma")]
    Comma,

    #[token(node = "Parenthesized<T>")]
    Parenthesis(TokenTree<ExampleLanguageToken>),

    /// Note: convertible to any other token type
    #[token(display_name = "<invalid token>")]
    #[fabricate]
    Invalid,
}

impl TokenKind for ExampleLanguageToken {
    fn next(cursor: &mut SrcCursor) -> ParseResult<Token<ExampleLanguageToken>> {
        let start = cursor.pos();

        // Check for EOF
        if cursor.peek().is_none() {
            return ParseResult::ok(Token::new_eof(None, cursor.span_from(start)));
        }

        // Identifiers & keywords
        if let Some(ident) = parse_c_like_word(cursor) {
            return ParseResult::ok(Token::new(Self::Ident(ident.into()), cursor.span_from(start)));
        }

        // Numbers
        if let Some((num, _)) = parse_c_like_num(cursor) {
            let mut errors = vec![];
            let res = match num.parse() {
                Ok(n) => n,
                Err(e) => {
                    errors.push(Error::new(format!("invalid number {num} ({e})"), cursor.span_from(start)));
                    0.0
                }
            };
            return ParseResult::new(Token::new(Self::Number(res), cursor.span_from(start)), errors);
        }

        // String literal
        if let Some(str) = parse_c_like_string(cursor) {
            let mut errors = vec![];
            let res = match str {
                ParsedString::Parsed(str) => str,
                ParsedString::InvalidEscapeSequences(seqs) => {
                    for seq in seqs {
                        errors.push(Error::new(format!("invalid escape sequence '\\{}'", seq.0), seq.1));
                    }
                    String::new()
                }
                ParsedString::Unterminated(span) => {
                    errors.push(Error::new("unterminated string literal", span));
                    String::new()
                }
            };
            return ParseResult::new(Token::new(Self::String(res), cursor.span_from(start)), errors);
        }

        if parse_exact(";", cursor).is_some() {
            return ParseResult::ok(Token::new(Self::Semicolon, cursor.span_from(start)));
        }
        if parse_exact(",", cursor).is_some() {
            return ParseResult::ok(Token::new(Self::Comma, cursor.span_from(start)));
        }

        // Operators
        if let Some(op) = parse_matching(is_op_char, cursor) {
            let mut errors = vec![];
            let res = match op {
                "==" => Self::Eq,
                "+" => Self::Plus,
                "-" => Self::Minus,
                "=" => Self::Assign,
                o => {
                    errors.push(Error::new(format!("invalid operator '{o}'"), cursor.span_from(start)));
                    Self::Invalid
                }
            };
            return ParseResult::new(Token::new(res, cursor.span_from(start)), errors);
        }

        // Parenthesis
        if let Some(tree) = parse_delimited("(", ")", cursor) {
            return tree.map(|tree| Token::new(Self::Parenthesis(tree), cursor.span_from(start)));
        }

        let next = cursor.next().unwrap();
        ParseResult::new(
            Token::new(ExampleLanguageToken::Invalid, cursor.span_from(start)),
            vec![Error::new(format!("invalid character '{}'", next), cursor.span_from(start))]
        )
    }
}

#[derive(Debug, NodeKind, Parse, Fabricate)]
#[parse(expected = "operator", token_type = "ExampleLanguageToken")]
pub enum BinOp {
    #[fabricate]
    Plus(PlusToken),
    Minus(MinusToken),
    Assign(AssignToken),
    Eq(EqToken),
}

impl BinOp {
    pub fn peek_precedence<I: TokenIterator<ExampleLanguageToken>>(tokenizer: &I) -> Option<usize> {
        match tokenizer.peek().as_token() {
            Some(token) => {
                match token {
                    ExampleLanguageToken::Plus => Some(2),
                    ExampleLanguageToken::Minus => Some(2),
                    ExampleLanguageToken::Eq => Some(1),
                    ExampleLanguageToken::Assign => Some(0),
                    _ => None,
                }
            }
            None => None
        }
    }
    pub fn peek_rtl<I: TokenIterator<ExampleLanguageToken>>(tokenizer: &I) -> Option<bool> {
        match tokenizer.peek().as_token() {
            Some(token) => {
                match token {
                    ExampleLanguageToken::Plus => Some(false),
                    ExampleLanguageToken::Minus => Some(false),
                    ExampleLanguageToken::Eq => Some(false),
                    ExampleLanguageToken::Assign => Some(true),
                    _ => None,
                }
            }
            None => None
        }
    }
}

#[derive(Debug, NodeKind, Parse, Fabricate)]
#[parse(expected = "expression", token_type = "ExampleLanguageToken")]
pub enum AtomExpr {
    Closed(Parenthesized<Box<Expr>>),
    String(StringToken),
    Number(NumberToken),
    #[fabricate]
    Ident(IdentToken),
}

#[derive(Debug, NodeKind, Fabricate)]
#[parse(expected = "expression", token_type = "ExampleLanguageToken")]
pub enum UnOp {
    Call {
        target: Box<Expr>,
        args: Parenthesized<SeparatedOptTrailing<Expr, CommaToken>>,
        span: Span,
    },
    #[fabricate]
    Atom(AtomExpr),
}

impl Parse<ExampleLanguageToken> for UnOp {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
        where Self: Sized, I: TokenIterator<ExampleLanguageToken>
    {
        let start = tokenizer.start();
        let (mut target, mut errors) = AtomExpr::parse(tokenizer).map(UnOp::Atom).into();
        loop {
            if <Parenthesized<SeparatedOptTrailing<Expr, CommaToken>>>::peek(tokenizer) {
                target = UnOp::Call {
                    target: Box::from(Expr::UnOp(target)),
                    args: <Parenthesized<SeparatedOptTrailing<Expr, CommaToken>>>::parse(tokenizer).append_and_get(&mut errors),
                    span: tokenizer.span_from(start),
                };
            }
            else {
                break;
            }
        }
        ParseResult::new(target, errors)
    }
    fn peek<I>(tokenizer: &I) -> bool
        where Self: Sized, I: TokenIterator<ExampleLanguageToken>
    {
        AtomExpr::peek(tokenizer)
    }
}

#[derive(Debug, NodeKind, Fabricate)]
#[parse(expected = "expression", token_type = "ExampleLanguageToken")]
pub enum Expr {
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
        span: Span,
    },
    #[fabricate]
    UnOp(UnOp),
}

impl Expr {
    fn parse_with_precedence<I>(tokenizer: &mut I, mut lhs: Self, current_prec: usize) -> ParseResult<Self>
        where I: TokenIterator<ExampleLanguageToken>
    {
        let mut errors = vec![];

        // Keep parsing binops with precedence
        loop {
            // This also checks that there's a binop coming up
            let Some(upcoming_prec) = BinOp::peek_precedence(tokenizer) else { break };
            if upcoming_prec < current_prec {
                break;
            }

            // Consume the upcoming operator
            // This should really not be capable of failing since 
            // BinOp::peek_precedence() returned Some
            let op = BinOp::parse(tokenizer).append_and_get(&mut errors);

            // Parse RHS
            let mut rhs = UnOp::parse(tokenizer).map(Expr::UnOp).append_and_get(&mut errors);

            // What does this mean????
            if let Some(true) = BinOp::peek_rtl(tokenizer) {
                rhs = Self::parse_with_precedence(tokenizer, rhs, current_prec).append_and_get(&mut errors);
            }

            if let Some(upcoming_2_prec) = BinOp::peek_precedence(tokenizer) {
                if upcoming_prec < upcoming_2_prec {
                    rhs = Self::parse_with_precedence(tokenizer, rhs, upcoming_2_prec).append_and_get(&mut errors);
                }
            }

            lhs = Expr::BinOp {
                span: tokenizer.span_from(lhs.span().start()),
                lhs: Box::from(lhs),
                op,
                rhs: Box::from(rhs),
            }
        }
        ParseResult::new(lhs, errors)
    }
}

impl Parse<ExampleLanguageToken> for Expr {
    fn parse<I>(tokenizer: &mut I) -> ParseResult<Self>
        where Self: Sized, I: TokenIterator<ExampleLanguageToken>
    {
        let lhs = UnOp::parse(tokenizer).map(Expr::UnOp);
        if BinOp::peek(tokenizer) {
            let (lhs, mut errors) = lhs.into();
            let res = Expr::parse_with_precedence(tokenizer, lhs, 0).append_and_get(&mut errors);
            ParseResult::new(res, errors)
        }
        else {
            lhs
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where Self: Sized, I: TokenIterator<ExampleLanguageToken>
    {
        AtomExpr::peek(tokenizer)
    }
}

#[derive(Debug, NodeKind, Parse, Fabricate)]
#[parse(token_type = "ExampleLanguageToken")]
pub struct ExprList {
    exprs: SeparatedOptTrailing<Expr, SemicolonToken>,
    span: Span,
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
    let tokens = src.tokenize::<ExampleLanguageToken>();
    if !tokens.errors().is_empty() {
        panic!("tokenization produced errors: {:?}", tokens.errors());
    }
    let ast = src.parse::<ExampleLanguageToken, ExprList>();
    if !ast.errors().is_empty() {
        panic!(
            "parsing produced errors:\n{}",
            ast.errors().iter().map(|e| e.display(&codebase)).collect::<Vec<String>>().join("\n")
        );
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
    if !src.tokenize::<ExampleLanguageToken>().errors().is_empty() {
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
    let tokens = src.tokenize::<ExampleLanguageToken>().assert_is_errorless();
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
