use logos::{Logos, SpannedIter};

#[derive(Debug, Clone, Logos)]
#[logos(skip(r"[ \t\n\f]+"))]
#[logos(skip(r"//[^\n]*", allow_greedy = true))]
pub enum Token {
    #[token("const")]
    Const,
    #[token("constructor")]
    Constructor,
    #[token("decl")]
    Decl,
    #[token("extern")]
    Extern,
    #[token("extractor")]
    Extractor,
    #[token("false")]
    False,
    #[token("if")]
    If,
    #[token("infallible")]
    Infallible,
    #[token("let")]
    Let,
    #[token("partial")]
    Partial,
    #[token("rule")]
    Rule,
    #[token("true")]
    True,
    #[token("type")]
    Type,

    #[regex(r"[\p{XID_Start}_]\p{XID_Continue}*", |lexer| lexer.slice().to_string())]
    Ident(String),
    #[regex(r"\d+", |lexer| lexer.slice().parse::<i64>().unwrap())]
    Integer(i64),

    #[token("&&")]
    And,
    #[token("->")]
    Arrow,
    #[token("=")]
    Assign,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("$")]
    Dollar,
    #[token("{")]
    LeftBracket,
    #[token("(")]
    LeftParen,
    #[token("-")]
    Minus,
    #[token("}")]
    RightBracket,
    #[token(")")]
    RightParen,
    #[token(";")]
    Semicolon,
    #[token("_", priority = 3)]
    Underscore,
}

pub type Spanned<Token, Location, Error> = Result<(Location, Token, Location), Error>;

pub struct Lexer<'input>(SpannedIter<'input, Token>);

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self(Token::lexer(input).spanned())
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|(token, span)| Ok((span.start, token?, span.end)))
    }
}
