use logos::{Logos, SpannedIter};

#[derive(Debug, Clone, PartialEq, Logos)]
#[logos(skip(r"[ \t\n\f]+"))]
#[logos(skip(r"//[^\n]*", allow_greedy = true))]
pub(crate) enum Token {
    #[token("const")]
    Const,
    #[token("extern")]
    Extern,
    #[token("false")]
    False,
    #[token("field")]
    Field,
    #[token("if")]
    If,
    #[token("let")]
    Let,
    #[token("multi")]
    Multi,
    #[token("overload")]
    Overload,
    #[token("partial")]
    Partial,
    #[token("pure")]
    Pure,
    #[token("rule")]
    Rule,
    #[token("term")]
    Term,
    #[token("total")]
    Total,
    #[token("true")]
    True,
    #[token("type")]
    Type,

    #[regex(r"[\p{XID_Start}_][\p{XID_Continue}]*", |lexer| lexer.slice().to_string())]
    Ident(String),
    #[regex(r"\d+", |lexer| lexer.slice().parse::<i64>().unwrap())]
    Integer(i64),
    #[regex(r"[0-9]+[a-z][a-z0-9]*", lex_suffixed_int)]
    SuffixedInteger((i64, String)),

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
    #[token(".")]
    Dot,
    #[token("{")]
    LeftBracket,
    #[token("(")]
    LeftParen,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token("}")]
    RightBracket,
    #[token(")")]
    RightParen,
    #[token(";")]
    Semicolon,
    #[token("_", priority = 3)]
    Underscore,
}

pub(crate) type Spanned<Token, Location, Error> = Result<(Location, Token, Location), Error>;

pub(crate) struct Lexer<'input>(SpannedIter<'input, Token>);

impl<'input> Lexer<'input> {
    pub(crate) fn new(input: &'input str) -> Self {
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

fn lex_suffixed_int(lexer: &mut logos::Lexer<Token>) -> Option<(i64, String)> {
    let slice = lexer.slice();
    let (digits, suffix) = slice.split_at(slice.find(|c: char| !c.is_ascii_digit())?);

    Some((digits.parse().ok()?, suffix.to_string()))
}
