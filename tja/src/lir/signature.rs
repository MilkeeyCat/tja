use crate::lir::Ty;

pub(crate) enum ValueKind {
    Normal,
    StructArgument(usize),
    StructReturn,
}

pub(crate) struct Value {
    pub(crate) ty: Ty,
    pub(crate) kind: ValueKind,
}

pub(crate) struct Signature {
    pub(crate) params: Vec<Value>,
    pub(crate) returns: Vec<Value>,
}
