#[derive(Debug)]
pub(crate) enum Definition {
    Declaration(Declaration),
    Extern(Extern),
    Type(Type),
    Rule(Rule),
}

#[derive(Debug)]
pub(crate) struct Declaration {
    pub(crate) name: String,
    pub(crate) arg_tys: Vec<String>,
    pub(crate) ret_ty: String,
    pub(crate) partial: bool,
}

#[derive(Debug)]
pub(crate) enum Extern {
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
pub(crate) enum Pattern {
    Application { name: String, args: Vec<Pattern> },
    Literal(Literal),
    Ident(String),
    Wildcard,
}

impl Pattern {
    pub(crate) fn is_wildcard(&self) -> bool {
        match self {
            Self::Ident(_) | Self::Wildcard => true,
            Self::Application { .. } | Self::Literal(_) => false,
        }
    }
}

#[derive(Debug)]
pub(crate) struct Type {
    pub(crate) name: String,
    pub(crate) external_name: String,
}

#[derive(Debug)]
pub(crate) struct Rule {
    pub(crate) pat: Pattern,
    pub(crate) priority: Option<i64>,
    pub(crate) guards: Vec<Guard>,
    pub(crate) body: Body,
}

#[derive(Debug)]
pub(crate) struct Let {
    pub(crate) name: String,
    pub(crate) ty: String,
    pub(crate) value: Expr,
}

#[derive(Debug)]
pub(crate) struct Body {
    pub(crate) lets: Vec<Let>,
    pub(crate) expr: Expr,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
    Call { name: String, args: Vec<Expr> },
    Literal(Literal),
    Ident(String),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
    Int(i64),
    Bool(bool),
    Const(String),
}

#[derive(Debug)]
pub(crate) enum Guard {
    Pattern(Pattern, Expr),
    Expr(Expr),
}
