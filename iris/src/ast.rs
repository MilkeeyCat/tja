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
}

#[derive(Debug)]
pub enum Extern {
    Constructor { name: String, external_name: String },
    Extractor { name: String, external_name: String },
    Const { name: String, ty: String },
}

#[derive(Debug)]
pub enum Pattern {
    Application { name: String, args: Vec<Pattern> },
    Literal(Literal),
    Ident(String),
}

#[derive(Debug)]
pub struct Type {
    pub name: String,
    pub external_name: String,
}

#[derive(Debug)]
pub struct Rule {
    pub pat: Pattern,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Call { name: String, args: Vec<Expr> },
    Literal(Literal),
    Ident(String),
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    Const(String),
}
