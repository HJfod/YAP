use std::path::Path;
use prolangine_macros::create_token_nodes;
use prolangine::{
    parse::{
        common::{
            parse_c_like_num,
            parse_c_like_string,
            parse_c_like_word,
            parse_delimited,
            parse_matching,
            CommonDelimiters,
            Parenthesized, ParsedString
        },
        node::{Node, NodeKind, Parse},
        token::{ParsedTokenKind, Token, TokenKind, TokenTree}
    },
    src::{Codebase, Span, SrcCursor}
};

fn is_op_char(ch: char) -> bool {
    matches!(ch, '+' | '-' | '=')
}

#[create_token_nodes]
pub enum ExampleLanguageToken {
    Number(f64),
    Ident(String),
    String(String),
    Plus,
    Minus,
    Assign,
    Eq,
    Parenthesis(TokenTree<ExampleLanguageToken>),
}

impl TokenKind for ExampleLanguageToken {
    // fn display_name(&self) -> String {
    //     match self {
    //         Self::Number(_) => String::from("number"),
    //         Self::Ident(_)  => String::from("identifier"),
    //         Self::String(_) => String::from("string"),
    //         Self::Op(_)     => String::from("operator"),
    //         Self::Parenthesis(_) => String::from("parenthesized expression"),
    //     }
    // }
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

impl CommonDelimiters for ExampleLanguageToken {
    fn is_parenthesized(&self) -> bool {
        matches!(self, Self::Parenthesis(_))
    }
    fn parenthesized(self) -> Option<TokenTree<Self>> {
        match self {
            Self::Parenthesis(p) => Some(p),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum AtomExpr {
    Closed(Node<Parenthesized<Expr>>),
    String(Node<StringToken>),
    Number(Node<NumberToken>),
}

impl Parse<ExampleLanguageToken> for AtomExpr {
    fn parse<I>(tokenizer: &mut I) -> Node<Self>
        where
            Self: Sized,
            I: prolangine::parse::token::TokenIterator<ExampleLanguageToken>
    {
        if Parenthesized::<Expr>::peek(tokenizer) {
            Node::from(Self::Closed(Parenthesized::parse(tokenizer)))
        }
        else if StringToken::peek(tokenizer) {
            Node::from(Self::String(StringToken::parse(tokenizer)))
        }
        else if NumberToken::peek(tokenizer) {
            Node::from(Self::Number(NumberToken::parse(tokenizer)))
        }
        else {
            let token = tokenizer.next();
            Node::expected("expression", &token, token.span())
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: prolangine::parse::token::TokenIterator<ExampleLanguageToken>
    {
        Parenthesized::<Expr>::peek(tokenizer) || 
            StringToken::peek(tokenizer) || 
            NumberToken::peek(tokenizer)
    }
}
impl NodeKind for AtomExpr {
    fn children(&self) -> Vec<&dyn NodeKind> {
        match self {
            Self::Closed(e) => e.children(),
            Self::String(e) => e.children(),
            Self::Number(e) => e.children(),
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::Closed(e) => e.span(),
            Self::String(e) => e.span(),
            Self::Number(e) => e.span(),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Atom(Node<AtomExpr>),
}

impl Parse<ExampleLanguageToken> for Expr {
    fn parse<I>(tokenizer: &mut I) -> Node<Self>
        where
            Self: Sized,
            I: prolangine::parse::token::TokenIterator<ExampleLanguageToken>
    {
        if AtomExpr::peek(tokenizer) {
            Node::from(Self::Atom(AtomExpr::parse(tokenizer)))
        }
        else {
            let token = tokenizer.next();
            Node::expected("expression", &token, token.span())
        }
    }
    fn peek<I>(tokenizer: &I) -> bool
        where
            Self: Sized,
            I: prolangine::parse::token::TokenIterator<ExampleLanguageToken>
    {
        AtomExpr::peek(tokenizer)
    }
}
impl NodeKind for Expr {
    fn children(&self) -> Vec<&dyn NodeKind> {
        match self {
            Self::Atom(e) => e.children(),
        }
    }
    fn span(&self) -> Span {
        match self {
            Self::Atom(e) => e.span(),
        }
    }
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
