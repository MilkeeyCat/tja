#[derive(Debug)]
pub enum Definition {
    Declaration(Declaration),
    Extern(Extern),
    Type(Type),
    Rule(Rule),
}

#[derive(Debug)]
pub struct Declaration {
    pub name: String,
    pub arg_tys: Vec<String>,
    pub ret_ty: String,
    pub partial: bool,
}

#[derive(Debug)]
pub enum Extern {
    Constructor {
        name: String,
        external_name: String,
    },
    Extractor {
        name: String,
        external_name: String,
        infallible: bool,
    },
    Const {
        name: String,
        ty: String,
    },
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Application { name: String, args: Vec<Pattern> },
    Literal(Literal),
    Ident(String),
    Wildcard,
}

impl Pattern {
    pub fn is_wildcard(&self) -> bool {
        match self {
            Self::Ident(_) | Self::Wildcard => true,
            Self::Application { .. } | Self::Literal(_) => false,
        }
    }
}

#[derive(Debug)]
pub struct Type {
    pub name: String,
    pub external_name: String,
}

#[derive(Debug)]
pub struct Rule {
    pub pat: Pattern,
    pub priority: Option<i64>,
    pub guards: Vec<Guard>,
    pub body: Body,
}

#[derive(Debug)]
pub struct Let {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Body {
    pub lets: Vec<Let>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Call { name: String, args: Vec<Expr> },
    Literal(Literal),
    Ident(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    Const(String),
}

#[derive(Debug, Clone)]
pub enum Guard {
    Pattern(Pattern, Expr),
    Expr(Expr),
}
