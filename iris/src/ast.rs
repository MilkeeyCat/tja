#[derive(Debug, PartialEq)]
pub(crate) enum Definition {
    Term(Term),
    Extern(Extern),
    Rule(Rule),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Term {
    Concrete(ConcreteTerm),
    Overload(OverloadTerm),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ConcreteTermKind {
    Partial,
    Total,
    Multi,
}

#[derive(Debug, PartialEq)]
pub(crate) struct ConcreteTerm {
    pub(crate) kind: ConcreteTermKind,
    pub(crate) name: String,
    pub(crate) arg_tys: Vec<Type>,
    pub(crate) ret_ty: Type,
    pub(crate) is_pure: bool,
}

#[derive(Debug, PartialEq)]
pub(crate) struct OverloadTerm {
    pub(crate) name: String,
    pub(crate) terms: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Type {
    Tuple(Vec<Self>),
    Ident(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum GlobalKind {
    Const,
    Field,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Extern {
    Type {
        name: String,
        external_name: Option<String>,
    },
    Term {
        term: ConcreteTerm,
        external_name: Option<String>,
    },
    Global {
        name: String,
        ty: Type,
        kind: GlobalKind,
        external_name: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum IntType {
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Literal {
    Int { value: i64, ty: Option<IntType> },
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Pattern {
    Application { name: String, args: Vec<Self> },
    Tuple(Vec<Self>),
    Literal(Literal),
    Ident(String),
    Wildcard,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
    Call { name: String, args: Vec<Self> },
    Tuple(Vec<Self>),
    TupleIndex { expr: Box<Self>, idx: usize },
    Literal(Literal),
    Ident(String),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Guard {
    pub(crate) pat: Pattern,
    pub(crate) expr: Expr,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Let {
    pub(crate) name: String,
    pub(crate) ty: Type,
    pub(crate) value: Expr,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Body {
    pub(crate) lets: Vec<Let>,
    pub(crate) expr: Expr,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Rule {
    pub(crate) pat: Pattern,
    pub(crate) guards: Vec<Guard>,
    pub(crate) body: Body,
}
