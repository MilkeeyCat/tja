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

pub(crate) struct ParamRanges(Vec<usize>);

impl ParamRanges {
    pub(crate) fn new(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity + 1))
    }

    pub(crate) fn add(&mut self, idx: usize) {
        assert!(self.0.len() < self.0.capacity());

        self.0.push(idx);
    }

    pub(crate) fn finalize(&mut self, idx: usize) {
        assert_eq!(self.0.len() + 1, self.0.capacity());

        self.0.push(idx);
    }

    pub(crate) fn get(&self, idx: usize) -> std::ops::Range<usize> {
        self.0[idx]..self.0[idx + 1]
    }
}
