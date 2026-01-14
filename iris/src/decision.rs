// algorithm based on Compiling Pattern Matching to Good Decision Trees
// http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf

use crate::{
    ast::{Expr, Literal, Pattern},
    lower::{BuiltinTy, Module, Rule, Type},
};
use index_vec::{IndexVec, define_index_type};
use std::collections::{HashMap, HashSet};

define_index_type! {
    pub struct ActionIdx = usize;
}

#[derive(Debug)]
pub struct Action<'a> {
    pub env: HashMap<String, OccurrenceIdx>,
    pub expr: &'a Expr,
}

define_index_type! {
    pub struct OccurrenceIdx = usize;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constructor {
    Int(i64),
    True,
    False,
    External(String),
    Pinned(String),
}

impl Constructor {
    fn arity(&self, module: &Module) -> usize {
        match self {
            Self::Int(_) | Self::True | Self::False | Self::Pinned(_) => 0,
            Self::External(name) => module.decls[name].arg_tys.len(),
        }
    }
}

impl Pattern {
    fn head_ctor(&self) -> Option<Constructor> {
        match self {
            Pattern::Application { name, .. } => Some(Constructor::External(name.clone())),
            Pattern::Literal(lit) => Some(match lit {
                Literal::Int(value) => Constructor::Int(*value),
                Literal::Bool(true) => Constructor::True,
                Literal::Bool(false) => Constructor::False,
                Literal::Const(name) => Constructor::Pinned(name.clone()),
            }),
            Pattern::Ident(_) | Pattern::Wildcard => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Decision {
    Leaf(ActionIdx),
    Fail,
    Switch(Vec<(Constructor, Decision)>, Option<Box<Decision>>),
    Swap(OccurrenceIdx, Box<Decision>),
}

#[derive(Debug)]
pub struct Match<'a> {
    pub tree: Decision,
    pub actions: IndexVec<ActionIdx, Action<'a>>,
}

#[derive(Debug, Clone)]
struct Row {
    pats: Vec<Pattern>,
    action_idx: ActionIdx,
}

#[derive(Debug)]
struct Matrix<'a> {
    module: &'a Module,
    rows: Vec<Row>,
}

impl<'a> Matrix<'a> {
    fn new(module: &'a Module) -> Self {
        Self {
            module,
            rows: Vec::new(),
        }
    }

    fn specialize(&mut self, ctor: Constructor) -> Self {
        let mut matrix = Self::new(self.module);

        for row in &self.rows {
            let pat = &row.pats[0];

            if let Some(pat_ctor) = pat.head_ctor()
                && pat_ctor != ctor
            {
                continue;
            }

            let mut new_row = Row {
                pats: Vec::new(),
                action_idx: row.action_idx,
            };

            match &row.pats[0] {
                Pattern::Application { args, .. } => {
                    new_row.pats.extend(args.iter().cloned());
                }
                Pattern::Ident(_) | Pattern::Wildcard => {
                    new_row
                        .pats
                        .extend(vec![Pattern::Wildcard; ctor.arity(self.module)]);
                }
                Pattern::Literal(_) => (),
            }

            new_row.pats.extend(row.pats.iter().skip(1).cloned());
            matrix.rows.push(new_row);
        }

        matrix
    }

    fn default(&mut self) -> Self {
        let mut matrix = Self::new(self.module);

        for row in &self.rows {
            let mut new_row = Row {
                pats: Vec::new(),
                action_idx: row.action_idx,
            };

            match &row.pats[0] {
                Pattern::Ident(_) | Pattern::Wildcard => {
                    new_row.pats.extend(row.pats.iter().skip(1).cloned())
                }
                Pattern::Application { .. } | Pattern::Literal(_) => continue,
            }

            matrix.rows.push(new_row);
        }

        matrix
    }

    fn compile(
        mut self,
        stack: &mut Vec<(&'a Type, OccurrenceIdx)>,
        actions: &mut IndexVec<ActionIdx, Action>,
        occurrence_idx: &mut OccurrenceIdx,
        has_fallback: bool,
    ) -> Decision {
        if self.rows.is_empty() {
            Decision::Fail
        } else if self.rows[0].pats.iter().all(|pat| pat.is_wildcard()) {
            let action = &mut actions[self.rows[0].action_idx];

            for (idx, pat) in self.rows[0].pats.iter().enumerate() {
                if let Pattern::Ident(name) = pat {
                    assert!(action.env.insert(name.to_string(), stack[idx].1).is_none());
                }
            }

            Decision::Leaf(self.rows[0].action_idx)
        } else {
            let idx = self.rows[0]
                .pats
                .iter()
                .enumerate()
                .find_map(|(idx, pat)| (!pat.is_wildcard()).then_some(idx))
                .unwrap();

            if idx == 0 {
                let mut default = None;
                let ctors: HashSet<_> = self
                    .rows
                    .iter()
                    .map(|row| row.pats[0].head_ctor())
                    .flatten()
                    .collect();

                for row in &self.rows {
                    if let Pattern::Ident(name) = &row.pats[0] {
                        assert!(
                            actions[row.action_idx]
                                .env
                                .insert(name.to_string(), stack[0].1)
                                .is_none()
                        );
                    }
                }

                let has_infallible_ctor = ctors.iter().any(|ctor| {
                    if let Constructor::External(name) = ctor {
                        self.module.decls[name]
                            .extractor
                            .as_ref()
                            .unwrap()
                            .infallible
                    } else {
                        false
                    }
                });

                if !is_sig_complete(stack[0].0, &ctors) && !has_infallible_ctor {
                    let element = stack.remove(0);
                    let decision =
                        self.default()
                            .compile(stack, actions, occurrence_idx, has_fallback);

                    if !(has_fallback && decision == Decision::Fail) {
                        default = Some(Box::new(decision));
                    }

                    stack.insert(0, element);
                }

                let mut cases = Vec::new();
                let element = stack.remove(0);

                for ctor in ctors {
                    if let Constructor::External(name) = &ctor {
                        stack.splice(
                            0..0,
                            self.module.decls[name].arg_tys.iter().map(|ty| {
                                let idx = *occurrence_idx;

                                *occurrence_idx += 1;

                                (ty, idx)
                            }),
                        );
                    }

                    cases.push((
                        ctor.clone(),
                        self.specialize(ctor.clone()).compile(
                            stack,
                            actions,
                            occurrence_idx,
                            has_fallback,
                        ),
                    ));

                    if let Constructor::External(name) = &ctor {
                        stack.drain(0..self.module.decls[name].arg_tys.len());
                    }
                }

                stack.insert(0, element);

                Decision::Switch(cases, default)
            } else {
                stack.swap(0, idx);

                for row in &mut self.rows {
                    row.pats.swap(0, idx);
                }

                Decision::Swap(
                    idx.into(),
                    Box::new(self.compile(stack, actions, occurrence_idx, has_fallback)),
                )
            }
        }
    }
}

fn is_sig_complete(ty: &Type, ctors: &HashSet<Constructor>) -> bool {
    match ty {
        Type::Builtin(BuiltinTy::Bool) => ctors.is_superset(
            &[Constructor::False, Constructor::True]
                .into_iter()
                .collect(),
        ),
        _ => false,
    }
}

pub fn compile<'a>(
    module: &'a Module,
    rules: &'a [Rule],
    pat_tys: &'a [Type],
    has_fallback: bool,
) -> Match<'a> {
    let mut occurrence_idx = OccurrenceIdx::new(0);
    let mut actions = IndexVec::<ActionIdx, _>::new();
    let mut matrix = Matrix::new(module);

    for rule in rules {
        let idx = actions.push(Action {
            env: HashMap::new(),
            expr: &rule.expr,
        });

        matrix.rows.push(Row {
            pats: rule.args.clone(),
            action_idx: idx,
        });
    }

    Match {
        tree: matrix.compile(
            &mut pat_tys
                .iter()
                .map(|ty| {
                    let idx = occurrence_idx;

                    occurrence_idx += 1;

                    (ty, idx)
                })
                .collect(),
            &mut actions,
            &mut occurrence_idx,
            has_fallback,
        ),
        actions,
    }
}
