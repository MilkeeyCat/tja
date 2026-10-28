use crate::late::ExprIdx;
use std::collections::HashMap;

#[derive(Debug)]
struct Scope {
    variables: HashMap<ExprIdx, usize>,
    next_var_idx: usize,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            next_var_idx: 0,
        }
    }

    fn new_child(parent: &Self) -> Self {
        Self {
            variables: HashMap::new(),
            next_var_idx: parent.next_var_idx,
        }
    }

    fn bind(&mut self, expr: ExprIdx) {
        assert!(self.variables.insert(expr, self.next_var_idx).is_none());

        self.next_var_idx += 1;
    }

    fn get(&self, expr: ExprIdx) -> Option<usize> {
        self.variables.get(&expr).copied()
    }
}

#[derive(Debug)]
pub(super) struct Scopes(Vec<Scope>);

impl Scopes {
    pub(super) fn new() -> Self {
        Self(vec![Scope::new()])
    }

    pub(super) fn enter(&mut self) {
        self.0.push(Scope::new_child(self.0.last().unwrap()));
    }

    pub(super) fn leave(&mut self) {
        assert!(self.0.pop().is_some());
    }

    pub(super) fn bind(&mut self, expr: ExprIdx) {
        self.0.last_mut().unwrap().bind(expr);
    }

    pub(super) fn get(&self, expr: ExprIdx) -> Option<usize> {
        self.0.iter().rev().find_map(|scope| scope.get(expr))
    }

    pub(super) fn next_var_idx(&self) -> usize {
        self.0.last().unwrap().next_var_idx
    }
}
