use crate::{
    ast::{Body, Guard, Literal, Pattern},
    lower::{
        BuiltinTy, ConstIdx, DeclIdx, EnvIdx, Expr, ExprIdx, Rule, RuleIdx, RuleSetLowerer, TyIdx,
        Type,
    },
};
use index_vec::IndexVec;
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Constructor {
    Int(i64),
    Bool(bool),
    External(DeclIdx),
    Pinned(ConstIdx),
}

impl Constructor {
    fn arity(&self, lowerer: &RuleSetLowerer) -> usize {
        match self {
            Self::Int(_) | Self::Bool(_) | Self::Pinned(_) => 0,
            Self::External(decl) => lowerer.lowerer.decls[*decl].arg_tys.len(),
        }
    }
}

impl Pattern {
    fn head_ctor(&self, lowerer: &RuleSetLowerer) -> Option<Constructor> {
        match self {
            Pattern::Application { name, .. } => {
                Some(Constructor::External(lowerer.lowerer.decl_map[name]))
            }
            Pattern::Literal(lit) => Some(match lit {
                Literal::Int(value) => Constructor::Int(*value),
                Literal::Bool(value) => Constructor::Bool(*value),
                Literal::Const(name) => Constructor::Pinned(lowerer.lowerer.const_map[name]),
            }),
            Pattern::Ident(_) | Pattern::Wildcard => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Condition {
    Eq(ExprIdx, ExprIdx),
    Some(ExprIdx, ExprIdx),
}

#[derive(Debug, PartialEq)]
pub enum Decision {
    If {
        condition: Condition,
        then: Box<Decision>,
        otherwise: Option<Box<Decision>>,
    },
    Let {
        expr: ExprIdx,
        decision: Box<Decision>,
    },
    Return {
        bindings: Vec<ExprIdx>,
        expr: ExprIdx,
    },
    Sequence(Vec<Decision>),
    Fail,
}

impl Decision {
    fn from_expr(lowerer: &RuleSetLowerer, expr: ExprIdx, inner: Decision) -> Decision {
        match &lowerer.exprs[expr] {
            Expr::Integer { .. }
            | Expr::Parameter(_)
            | Expr::Bool(_)
            | Expr::Const(_)
            | Expr::Extractor { .. }
            | Expr::MakeSome(_)
            | Expr::TupleIndex { .. } => inner,
            Expr::Constructor { args, .. } => args.iter().fold(inner, |decision, expr| {
                Self::from_expr(lowerer, *expr, decision)
            }),
            Expr::MatchSome(some) => Self::from_expr(
                lowerer,
                *some,
                Decision::If {
                    condition: Condition::Some(expr, *some),
                    then: Box::new(inner),
                    otherwise: None,
                },
            ),
        }
    }
}

struct Row<'a> {
    env: EnvIdx,
    pats: Vec<&'a Pattern>,
    guards: &'a [Guard],
    body: &'a Body,
}

impl Row<'_> {
    fn is_wildcard(&self) -> bool {
        self.pats.iter().all(|pat| pat.is_wildcard())
    }

    fn non_wildcard_pat_idx(&self) -> Option<usize> {
        self.pats
            .iter()
            .enumerate()
            .find_map(|(idx, pat)| (!pat.is_wildcard()).then_some(idx))
    }
}

struct Matrix<'a, 'b> {
    lowerer: &'a mut RuleSetLowerer<'b>,
    rows: Vec<Row<'a>>,
}

impl<'a, 'b> Matrix<'a, 'b> {
    fn new(lowerer: &'a mut RuleSetLowerer<'b>) -> Self {
        Self {
            lowerer,
            rows: Vec::new(),
        }
    }

    fn new_with_rows(lowerer: &'a mut RuleSetLowerer<'b>, rows: Vec<Row<'a>>) -> Self {
        Self { lowerer, rows }
    }

    fn non_wildcard_row_idx(&self) -> Option<usize> {
        self.rows
            .iter()
            .enumerate()
            .find_map(|(idx, row)| (!row.pats.iter().all(|pat| pat.is_wildcard())).then_some(idx))
    }

    fn specialize(&mut self, ctor: &Constructor) -> Matrix<'_, 'b> {
        let mut rows = Vec::new();

        for row in &self.rows {
            let pat = row.pats[0];

            if pat.head_ctor(self.lowerer).as_ref() != Some(ctor) {
                continue;
            }

            let mut new_row = Row {
                env: row.env,
                pats: Vec::new(),
                guards: row.guards,
                body: row.body,
            };

            match pat {
                Pattern::Application { args, .. } => {
                    new_row.pats.extend(args.iter());
                }
                Pattern::Ident(_) | Pattern::Wildcard => {
                    new_row
                        .pats
                        .extend(vec![&Pattern::Wildcard; ctor.arity(self.lowerer)]);
                }
                Pattern::Literal(_) => (),
            }

            new_row.pats.extend(row.pats.iter().skip(1).cloned());
            rows.push(new_row);
        }

        Matrix::new_with_rows(self.lowerer, rows)
    }

    fn default(&mut self) -> Matrix<'_, 'b> {
        let mut rows = Vec::new();

        for row in &self.rows {
            let mut new_row = Row {
                env: row.env,
                pats: Vec::new(),
                guards: row.guards,
                body: row.body,
            };

            match &row.pats[0] {
                Pattern::Ident(_) | Pattern::Wildcard => {
                    new_row.pats.extend(row.pats.iter().skip(1).cloned())
                }
                Pattern::Application { .. } | Pattern::Literal(_) => continue,
            }

            rows.push(new_row);
        }

        Matrix::new_with_rows(self.lowerer, rows)
    }

    fn compile(mut self, mut exprs: Vec<ExprIdx>, has_fallback: bool) -> Decision {
        if self.rows.is_empty() {
            Decision::Fail
        } else if self.rows[0].is_wildcard() {
            if let Some(idx) = self.non_wildcard_row_idx() {
                self.rows.swap(0, idx);

                self.compile(exprs, has_fallback)
            } else {
                // TODO: check that there's only 1 row without guards and if
                // it's true make sure that it's the last row
                let mut decisions = Vec::new();

                for mut row in self.rows {
                    match row.guards {
                        [] => {
                            for (idx, pat) in row.pats.iter().enumerate() {
                                if let Pattern::Ident(name) = pat {
                                    self.lowerer.bind_var(row.env, name.clone(), exprs[idx]);
                                }
                            }

                            let ty = self.lowerer.lowerer.decls[self.lowerer.decl].ret_ty;
                            let (bindings, expr) = self.lowerer.lower_body(row.env, row.body, ty);

                            return Decision::Return { bindings, expr };
                        }
                        [guard, rest @ ..] => {
                            let (pat, expr) = match guard {
                                Guard::Pattern(pat, expr) => (pat, expr),
                                Guard::Expr(expr) => (&Pattern::Wildcard, expr),
                            };
                            let expr = self.lowerer.lower_expr(expr, row.env, None);

                            row.guards = rest;
                            row.pats.insert(0, pat);

                            let matrix = Matrix::new_with_rows(self.lowerer, vec![row]);
                            let decision = matrix.compile(
                                exprs
                                    .clone()
                                    .into_iter()
                                    .chain(std::iter::once(expr))
                                    .collect(),
                                true,
                            );

                            decisions.push(Decision::from_expr(self.lowerer, expr, decision));
                        }
                    }
                }

                if !has_fallback {
                    decisions.push(Decision::Fail);
                }

                Decision::Sequence(decisions)
            }
        } else {
            let idx = self.rows[0].non_wildcard_pat_idx().unwrap();

            if idx != 0 {
                exprs.swap(0, idx);

                for row in &mut self.rows {
                    row.pats.swap(0, idx);
                }

                return self.compile(exprs, has_fallback);
            }

            let mut default = None;
            let ty = self.lowerer.expr_ty(exprs[0]).unwrap();
            let mut ctors: BTreeSet<_> = self
                .rows
                .iter()
                .map(|row| row.pats[0].head_ctor(self.lowerer))
                .flatten()
                .collect();
            let mut infallible_ctors = ctors.iter().filter(|ctor| {
                matches!(
                    ctor,
                    Constructor::External(decl)
                        if self.lowerer
                            .lowerer
                            .decls[*decl]
                            .extractor
                            .as_ref()
                            .unwrap()
                            .infallible
                )
            });

            if let Some(ctor) = infallible_ctors.next().cloned() {
                assert!(
                    infallible_ctors.next().is_none(),
                    "at most 1 pattern be be non-fallible"
                );

                ctors.remove(&ctor);

                default =
                    Some(self.compile_specialized(exprs.clone(), ctor, ty, has_fallback, None));
            }

            for row in &self.rows {
                if let Pattern::Ident(name) = &row.pats[0] {
                    self.lowerer.bind_var(row.env, name.clone(), exprs[0]);
                }
            }

            if default.is_none() && !is_sig_complete(&self.lowerer.lowerer.types[ty], &ctors) {
                let decision = self
                    .default()
                    .compile(exprs.clone().into_iter().skip(1).collect(), has_fallback);

                if !(has_fallback && decision == Decision::Fail) {
                    default = Some(decision);
                }
            }

            ctors
                .into_iter()
                .rfold(default, |otherwise, ctor| {
                    Some(self.compile_specialized(exprs.clone(), ctor, ty, has_fallback, otherwise))
                })
                .unwrap()
        }
    }

    fn compile_specialized(
        &mut self,
        mut exprs: Vec<ExprIdx>,
        ctor: Constructor,
        ty: TyIdx,
        has_fallback: bool,
        otherwise: Option<Decision>,
    ) -> Decision {
        let expr = exprs[0];

        exprs.splice(0..1, expand_expr(self.lowerer, &ctor, expr));

        let then = self.specialize(&ctor).compile(exprs, has_fallback);
        let condition = match ctor {
            Constructor::Int(value) => {
                Condition::Eq(expr, self.lowerer.create_expr(Expr::Integer { value, ty }))
            }
            Constructor::Bool(value) => {
                Condition::Eq(expr, self.lowerer.create_expr(Expr::Bool(value)))
            }
            Constructor::External(decl) => {
                let expr = self
                    .lowerer
                    .create_expr(Expr::Extractor { decl, arg: expr });

                if self.lowerer.lowerer.decls[decl]
                    .extractor
                    .as_ref()
                    .unwrap()
                    .infallible
                {
                    return Decision::Let {
                        expr,
                        decision: Box::new(then),
                    };
                }

                Condition::Some(self.lowerer.create_expr(Expr::MatchSome(expr)), expr)
            }
            Constructor::Pinned(name) => {
                Condition::Eq(expr, self.lowerer.create_expr(Expr::Const(name)))
            }
        };

        Decision::If {
            condition,
            then: Box::new(then),
            otherwise: otherwise.map(Box::new),
        }
    }
}

fn expand_expr(lowerer: &mut RuleSetLowerer, ctor: &Constructor, expr: ExprIdx) -> Vec<ExprIdx> {
    match ctor {
        Constructor::Int(_) | Constructor::Bool(_) | Constructor::Pinned(_) => Vec::new(),
        &Constructor::External(decl) => {
            let mut expr = lowerer.create_expr(Expr::Extractor {
                decl: decl,
                arg: expr,
            });

            if !lowerer.lowerer.decls[decl]
                .extractor
                .as_ref()
                .unwrap()
                .infallible
            {
                expr = lowerer.create_expr(Expr::MatchSome(expr));
            }

            match lowerer.lowerer.decls[decl].arg_tys.len() {
                1 => vec![expr],
                n => (0..n)
                    .into_iter()
                    .map(|idx| lowerer.create_expr(Expr::TupleIndex { expr, idx }))
                    .collect(),
            }
        }
    }
}

fn is_sig_complete(ty: &Type, ctors: &BTreeSet<Constructor>) -> bool {
    match ty {
        Type::Builtin(BuiltinTy::Bool) => ctors.is_superset(
            &[Constructor::Bool(true), Constructor::Bool(false)]
                .into_iter()
                .collect(),
        ),
        _ => false,
    }
}

pub fn compile(lowerer: &mut RuleSetLowerer, rules: IndexVec<RuleIdx, Rule>) -> Decision {
    let exprs = (0..lowerer.lowerer.decls[lowerer.decl].arg_tys.len())
        .map(|idx| lowerer.create_expr(Expr::Parameter(idx)))
        .collect();
    let mut matrix = Matrix::new(lowerer);

    matrix.rows = rules
        .iter_enumerated()
        .map(|(idx, rule)| Row {
            env: idx,
            pats: rule.args.iter().collect(),
            guards: rule.guards,
            body: rule.body,
        })
        .collect();

    matrix.compile(exprs, false)
}
