use crate::{
    TermDeclIdx,
    ast::ConcreteTermKind,
    sema::{self, Body, Global, GlobalIdx, Guard, Literal, Pattern, Rule, VariableIdx},
};
use index_vec::IndexVec;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use tja_macros::define_index_type;

pub(crate) struct Program {
    pub(crate) globals: IndexVec<GlobalIdx, Global>,
    pub(crate) term_decls: IndexVec<TermDeclIdx, TermDecl>,
    pub(crate) sema_types: IndexVec<sema::TypeIdx, sema::Type>,
    pub(crate) late_types: IndexVec<TypeIdx, Type>,
    pub(crate) exprs: IndexVec<ExprIdx, Expr>,
    pub(crate) terms: BTreeMap<TermDeclIdx, Block>,
}

pub(crate) struct TermDecl {
    pub(crate) name: String,
    pub(crate) arg_tys: Vec<sema::TypeIdx>,
    pub(crate) ret_ty: TypeIdx,
    pub(crate) is_external: bool,
    is_pure: bool,
}

define_index_type! {
    pub(crate) struct TypeIdx = usize;
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct Type {
    pub(crate) ty: sema::TypeIdx,
    pub(crate) cardinality: TypeCardinality,
}

impl Type {
    fn option(ty: sema::TypeIdx) -> Self {
        Self {
            ty,
            cardinality: TypeCardinality::Option,
        }
    }

    fn scalar(ty: sema::TypeIdx) -> Self {
        Self {
            ty,
            cardinality: TypeCardinality::Scalar,
        }
    }

    fn iterator(ty: sema::TypeIdx) -> Self {
        Self {
            ty,
            cardinality: TypeCardinality::Iterator,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum TypeCardinality {
    Option,
    Scalar,
    Iterator,
}

define_index_type! {
    pub(crate) struct ExprIdx = usize;
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct Expr {
    pub(crate) kind: ExprKind,
    pub(crate) ty: TypeIdx,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) enum ExprKind {
    Call {
        term_decl: TermDeclIdx,
        args: Vec<ExprIdx>,
    },
    Global(GlobalIdx),
    Literal(Literal),
    Parameter(usize),
    TupleIndex {
        expr: ExprIdx,
        idx: usize,
    },
    Tuple(Vec<ExprIdx>),
    Next(ExprIdx),
    MatchSome(ExprIdx),
    MakeSome(ExprIdx),
}

pub(crate) struct Block(pub(crate) Vec<EvalStep>);

pub(crate) struct EvalStep {
    pub(crate) bindings: Vec<ExprIdx>,
    pub(crate) ctrl_flow: ControlFlow,
}

impl From<EvalStep> for Vec<EvalStep> {
    fn from(step: EvalStep) -> Self {
        vec![step]
    }
}

pub(crate) enum ControlFlow {
    Match {
        target: ExprIdx,
        arms: Vec<MatchArm>,
    },
    Loop {
        item: ExprIdx,
        iterator: ExprIdx,
        body: Block,
    },
    Return(ExprIdx),
    Bail,
}

pub(crate) struct MatchArm {
    pub(crate) constraint: Constraint,
    pub(crate) bindings: Vec<ExprIdx>,
    pub(crate) body: Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Constructor {
    Bool(bool),
    Global(GlobalIdx),
    Int(i64),
    Local(ExprIdx),
    Term(TermDeclIdx),
    Tuple,
}

impl Constructor {
    /// Can instances of this constructor be grouped together?
    fn is_groupable(&self) -> bool {
        matches!(
            self,
            Constructor::Bool(_) | Constructor::Global(_) | Constructor::Int(_)
        )
    }
}

impl Pattern {
    fn is_wildcard(&self) -> bool {
        matches!(self, Self::Bind(_) | Self::Wildcard)
    }
}

define_index_type! {
    struct RowIdx = usize;
}

struct Row {
    variables: HashMap<VariableIdx, ExprIdx>,
    pats: Vec<Pattern>,
    guards: Vec<Guard>,
    body: Option<Body>,
}

impl Row {
    // maybe 'resolved' isn't a good name, but it's the best I've come up with
    fn is_resolved(&self) -> bool {
        self.pats.iter().all(Pattern::is_wildcard) && self.guards.is_empty()
    }

    fn non_wildcard_col_idx(&self) -> Option<usize> {
        self.pats.iter().position(|pat| !pat.is_wildcard())
    }

    fn pat_head_ctor(&self) -> Option<Constructor> {
        match self.pats.first()? {
            &Pattern::Application { term_decl, .. } => Some(Constructor::Term(term_decl)),
            Pattern::Tuple(_) => Some(Constructor::Tuple),
            &Pattern::Literal(Literal::Int(value)) => Some(Constructor::Int(value)),
            &Pattern::Literal(Literal::Bool(value)) => Some(Constructor::Bool(value)),
            Pattern::Variable(var) => Some(Constructor::Local(self.variables[var])),
            &Pattern::Global(global) => Some(Constructor::Global(global)),
            Pattern::Bind(_) | Pattern::Wildcard => None,
        }
    }

    fn bind(&mut self, var: VariableIdx, expr: ExprIdx) {
        assert!(self.variables.insert(var, expr).is_none());
    }

    fn is_bound(&self, expr: ExprIdx) -> bool {
        self.variables.iter().any(|(_, &e)| e == expr)
    }
}

pub(crate) enum Constraint {
    Bool(bool),
    Global(GlobalIdx),
    Int(i64),
    Local(ExprIdx),
    Some,
}

enum CompilationAction {
    Specialize,
    CompileGuard(Vec<RowIdx>),
}

struct Compiler<'a, 'b> {
    ctx: &'a mut CompilationContext<'b>,
    decl: TermDeclIdx,
    cols: Vec<ExprIdx>,
    rows: IndexVec<RowIdx, Row>,
}

impl<'a, 'b> Compiler<'a, 'b> {
    fn new(ctx: &'a mut CompilationContext<'b>, decl: TermDeclIdx, rules: Vec<Rule>) -> Self {
        let cols = ctx.term_decls[decl]
            .arg_tys
            .clone()
            .into_iter()
            .enumerate()
            .map(|(idx, ty)| {
                let ty = ctx.intern_type(Type::scalar(ty));

                ctx.intern_expr(Expr {
                    kind: ExprKind::Parameter(idx),
                    ty,
                })
            })
            .collect();
        let rows = rules
            .into_iter()
            .map(|Rule { pats, guards, body }| Row {
                variables: pats
                    .iter()
                    .zip(&cols)
                    .filter_map(|(pat, &expr)| {
                        if let &Pattern::Bind(var) = pat {
                            Some((var, expr))
                        } else {
                            None
                        }
                    })
                    .collect(),
                pats,
                guards,
                body: Some(body),
            })
            .collect();

        Self {
            ctx,
            decl,
            cols,
            rows,
        }
    }

    fn next_action(&self) -> CompilationAction {
        let ctors = self.rows.iter().filter_map(Row::pat_head_ctor).count();
        let mut guards: Vec<(SemaExprEnv, Vec<RowIdx>)> = Vec::new();

        for (idx, row) in self.rows.iter_enumerated() {
            if let Some(Guard { expr, .. }) = row.guards.first() {
                let expr = SemaExprEnv::new(expr, &row.variables);

                if expr.all_deps_computed() {
                    match guards.iter().position(|(e, _)| e == &expr) {
                        Some(pos) => {
                            guards[pos].1.push(idx);
                        }
                        None => {
                            guards.push((expr, vec![idx]));
                        }
                    }
                }
            }
        }

        match guards
            .into_iter()
            .map(|(_, rows)| rows)
            .max_by_key(Vec::len)
        {
            Some(rows) if rows.len() > ctors => CompilationAction::CompileGuard(rows),
            _ => CompilationAction::Specialize,
        }
    }

    fn compile(&mut self) -> Vec<EvalStep> {
        assert!(!self.rows.is_empty());

        let row = &self.rows[0];

        if row.is_resolved() {
            if let Some(row) = self.unresolved_row() {
                self.rows.swap(RowIdx::from_usize(0), row);

                return self.compile();
            }

            assert!(
                self.ctx.late_types[self.ctx.term_decls[self.decl].ret_ty].cardinality
                    == TypeCardinality::Iterator
                    || self.rows.len() == 1,
                "overlapping rules"
            );

            let step = self.compile_body();
            let bail = !(self.ctx.late_types[self.ctx.term_decls[self.decl].ret_ty].cardinality
                == TypeCardinality::Iterator
                || matches!(step.ctrl_flow, ControlFlow::Return(_)));
            let mut steps = vec![step];

            if bail {
                steps.push(EvalStep {
                    bindings: Vec::new(),
                    ctrl_flow: ControlFlow::Bail,
                });
            }

            return steps;
        }

        let mut steps = match self.next_action() {
            CompilationAction::Specialize => {
                if let Some(idx) = row.non_wildcard_col_idx()
                    && idx != 0
                {
                    self.swap(idx);
                }

                let ctors = self.rows.iter().filter_map(Row::pat_head_ctor).collect();

                group_ctors(ctors)
                    .into_iter()
                    .flat_map(|group| self.compile_group(group))
                    .collect()
            }
            CompilationAction::CompileGuard(rows) => self.compile_guard(rows),
        };

        if !self.rows.is_empty() {
            steps.extend(self.default().compile());
        }

        steps
    }

    fn compile_group(&mut self, group: Vec<Constructor>) -> Vec<EvalStep> {
        let mut arms = Vec::new();

        for ctor in group {
            match ctor {
                Constructor::Bool(value) => {
                    arms.push(MatchArm {
                        constraint: Constraint::Bool(value),
                        bindings: Vec::new(),
                        body: Block(self.specialize(ctor, self.cols[0]).compile()),
                    });
                }
                Constructor::Global(global) => {
                    arms.push(MatchArm {
                        constraint: Constraint::Global(global),
                        bindings: Vec::new(),
                        body: Block(self.specialize(ctor, self.cols[0]).compile()),
                    });
                }
                Constructor::Int(value) => {
                    arms.push(MatchArm {
                        constraint: Constraint::Int(value),
                        bindings: Vec::new(),
                        body: Block(self.specialize(ctor, self.cols[0]).compile()),
                    });
                }
                Constructor::Local(expr) => {
                    arms.push(MatchArm {
                        constraint: Constraint::Local(expr),
                        bindings: Vec::new(),
                        body: Block(self.specialize(ctor, self.cols[0]).compile()),
                    });
                }
                Constructor::Term(decl) => {
                    let ret_ty = self.ctx.term_decls[decl].ret_ty;
                    let call = self.ctx.intern_expr(Expr {
                        kind: ExprKind::Call {
                            term_decl: decl,
                            args: vec![self.cols[0]],
                        },
                        ty: ret_ty,
                    });

                    return match self.ctx.late_types[ret_ty].cardinality {
                        TypeCardinality::Option => {
                            let ty = self
                                .ctx
                                .intern_type(Type::scalar(self.ctx.late_types[ret_ty].ty));
                            let matched = self.ctx.intern_expr(Expr {
                                kind: ExprKind::MatchSome(call),
                                ty,
                            });

                            vec![EvalStep {
                                bindings: Vec::new(),
                                ctrl_flow: ControlFlow::Match {
                                    target: call,
                                    arms: vec![MatchArm {
                                        bindings: vec![matched],
                                        constraint: Constraint::Some,
                                        body: Block(self.specialize(ctor, matched).compile()),
                                    }],
                                },
                            }]
                        }
                        TypeCardinality::Scalar => self.specialize(ctor, call).compile(),
                        TypeCardinality::Iterator => {
                            let ty = self
                                .ctx
                                .intern_type(Type::option(self.ctx.late_types[ret_ty].ty));
                            let next = self.ctx.intern_expr(Expr {
                                kind: ExprKind::Next(call),
                                ty,
                            });
                            let item = self.ctx.intern_expr(Expr {
                                kind: ExprKind::MatchSome(next),
                                ty: ret_ty,
                            });

                            vec![EvalStep {
                                bindings: Vec::new(),
                                ctrl_flow: ControlFlow::Loop {
                                    item,
                                    iterator: call,
                                    body: Block(self.specialize(ctor, item).compile()),
                                },
                            }]
                        }
                    };
                }
                Constructor::Tuple => return self.specialize(ctor, self.cols[0]).compile(),
            }
        }

        vec![EvalStep {
            bindings: Vec::new(),
            ctrl_flow: ControlFlow::Match {
                target: self.cols[0],
                arms,
            },
        }]
    }

    fn specialize(&mut self, ctor: Constructor, expr: ExprIdx) -> Compiler<'_, 'b> {
        let ty = self.ctx.late_types[self.ctx.exprs[expr].ty].ty;
        let mut cols = match ctor {
            Constructor::Term(decl) => {
                match &self.ctx.sema_types[self.ctx.late_types[self.ctx.term_decls[decl].ret_ty].ty]
                {
                    sema::Type::Tuple(tys) => tys
                        .iter()
                        .enumerate()
                        .map(|(idx, &ty)| {
                            let ty = self.ctx.intern_type(Type::scalar(ty));

                            self.ctx.intern_expr(Expr {
                                kind: ExprKind::TupleIndex { expr, idx },
                                ty,
                            })
                        })
                        .collect(),
                    _ => vec![expr],
                }
            }
            Constructor::Tuple => match &self.ctx.sema_types[ty] {
                sema::Type::Tuple(tys) => tys
                    .iter()
                    .enumerate()
                    .map(|(idx, &ty)| {
                        let ty = self.ctx.intern_type(Type::scalar(ty));

                        self.ctx.intern_expr(Expr {
                            kind: ExprKind::TupleIndex { expr, idx },
                            ty,
                        })
                    })
                    .collect(),
                _ => unreachable!(),
            },
            Constructor::Bool(_)
            | Constructor::Global(_)
            | Constructor::Int(_)
            | Constructor::Local(_) => Vec::new(),
        };

        cols.extend_from_slice(&self.cols[1..]);

        let mut rows = IndexVec::new();

        for mut row in self
            .rows
            .raw
            .extract_if(.., |row| row.pat_head_ctor() == Some(ctor))
        {
            let arity = match ctor {
                Constructor::Bool(_)
                | Constructor::Global(_)
                | Constructor::Int(_)
                | Constructor::Local(_) => 0,
                Constructor::Term(decl) => {
                    match &self.ctx.sema_types
                        [self.ctx.late_types[self.ctx.term_decls[decl].ret_ty].ty]
                    {
                        sema::Type::Tuple(tys) => tys.len(),
                        _ => 1,
                    }
                }
                Constructor::Tuple => match &self.ctx.sema_types[ty] {
                    sema::Type::Tuple(tys) => tys.len(),
                    _ => unreachable!(),
                },
            };
            let pats = match row.pats.remove(0) {
                Pattern::Application { pats, .. } | Pattern::Tuple(pats) => pats,
                Pattern::Wildcard => vec![Pattern::Wildcard; arity],
                Pattern::Literal(_)
                | Pattern::Bind(_)
                | Pattern::Variable(_)
                | Pattern::Global(_) => Vec::new(),
            };

            for (idx, pat) in pats.iter().enumerate() {
                if let &Pattern::Bind(var) = pat {
                    row.bind(var, cols[idx]);
                }
            }

            row.pats.splice(0..0, pats);
            rows.push(row);
        }

        assert!(rows.iter().all(|row| row.pats.len() == cols.len()));

        Compiler {
            ctx: self.ctx,
            decl: self.decl,
            cols,
            rows,
        }
    }

    fn default(&mut self) -> Compiler<'_, 'b> {
        let cols = self.cols.iter().skip(1).copied().collect();
        let rows = self
            .rows
            .raw
            .extract_if(.., |row| match &row.pats[0] {
                Pattern::Bind(_) | Pattern::Wildcard => {
                    row.pats.remove(0);

                    true
                }
                Pattern::Application { .. }
                | Pattern::Tuple(_)
                | Pattern::Literal(_)
                | Pattern::Variable(_)
                | Pattern::Global(_) => false,
            })
            .collect();

        Compiler {
            ctx: self.ctx,
            decl: self.decl,
            cols,
            rows,
        }
    }

    fn compile_guard(&mut self, rows: Vec<RowIdx>) -> Vec<EvalStep> {
        let mut rows: IndexVec<_, _> = rows
            .into_iter()
            .rev()
            .map(|idx| self.rows.remove(idx))
            .collect();
        let mut expr = None;

        for (idx, row) in rows.iter_mut().enumerate() {
            let guard = row.guards.remove(0);

            if idx == 0 {
                expr = Some(guard.expr);
            }

            row.pats.insert(0, guard.pat);
        }

        Compiler {
            ctx: self.ctx,
            decl: self.decl,
            cols: self.cols.clone(),
            rows,
        }
        .compile_expr(expr.unwrap(), &mut |this, expr, _| {
            this.cols.insert(0, expr);

            this.compile()
        })
    }

    fn compile_body(&mut self) -> EvalStep {
        let body = self.rows[0].body.as_mut().unwrap();

        if body.bindings.is_empty() {
            let expr = self.rows[0].body.take().unwrap().result;

            self.compile_expr(expr, &mut |this, mut expr, _| {
                if this.ctx.late_types[this.ctx.term_decls[this.decl].ret_ty].cardinality
                    == TypeCardinality::Option
                {
                    let ty = this.ctx.intern_type(Type::scalar(
                        this.ctx.late_types[this.ctx.term_decls[this.decl].ret_ty].ty,
                    ));

                    expr = this.ctx.intern_expr(Expr {
                        kind: ExprKind::MakeSome(expr),
                        ty,
                    });
                }

                EvalStep {
                    bindings: Vec::new(),
                    ctrl_flow: ControlFlow::Return(expr),
                }
            })
        } else {
            let (var, expr) = body.bindings.remove(0);

            self.compile_expr(expr, &mut |this, expr, should_bind| {
                this.rows[0].bind(var, expr);

                let mut step = this.compile_body();

                if should_bind {
                    step.bindings.insert(0, expr);
                }

                step
            })
        }
    }

    // recursion is hard af -.-
    fn compile_exprs<
        R: From<EvalStep> + Into<Vec<EvalStep>>,
        F: FnMut(&mut Compiler, Vec<ExprIdx>) -> R,
    >(
        &mut self,
        exprs: Vec<sema::Expr>,
        mut finish: F,
    ) -> R {
        fn compile_exprs<
            I: Iterator<Item = sema::Expr>,
            R: From<EvalStep> + Into<Vec<EvalStep>>,
            F: FnMut(&mut Compiler, Vec<ExprIdx>) -> R,
        >(
            compiler: &mut Compiler,
            exprs: &mut I,
            compiled: &mut Vec<ExprIdx>,
            finish: &mut F,
        ) -> R {
            match exprs.next() {
                Some(expr) => compiler.compile_expr(expr, &mut |this, expr, _| {
                    compiled.push(expr);

                    compile_exprs(this, exprs, compiled, finish)
                }),
                None => finish(compiler, std::mem::take(compiled)),
            }
        }

        let len = exprs.len();

        compile_exprs(
            self,
            &mut exprs.into_iter(),
            &mut Vec::with_capacity(len),
            &mut finish,
        )
    }

    fn compile_expr<R: From<EvalStep> + Into<Vec<EvalStep>>>(
        &mut self,
        expr: sema::Expr,
        // NOTE: pass the closure using `dyn` to avoid infinite method instantiating
        then: &mut dyn FnMut(&mut Compiler, ExprIdx, bool) -> R,
    ) -> R {
        match expr.kind {
            sema::ExprKind::Call { term_decl, args } => self.compile_exprs(args, |this, args| {
                let ty = this.ctx.term_decls[term_decl].ret_ty;
                let expr = this.ctx.intern_expr(Expr {
                    kind: ExprKind::Call { term_decl, args },
                    ty,
                });

                if this.rows[0].is_bound(expr) {
                    return then(this, expr, false);
                }

                match this.ctx.late_types[ty].cardinality {
                    TypeCardinality::Option => {
                        let ty = this
                            .ctx
                            .intern_type(Type::scalar(this.ctx.late_types[ty].ty));
                        let some = this.ctx.intern_expr(Expr {
                            kind: ExprKind::MatchSome(expr),
                            ty,
                        });

                        EvalStep {
                            bindings: Vec::new(),
                            ctrl_flow: ControlFlow::Match {
                                target: expr,
                                arms: vec![MatchArm {
                                    constraint: Constraint::Some,
                                    bindings: vec![some],
                                    body: Block(then(this, some, false).into()),
                                }],
                            },
                        }
                        .into()
                    }
                    TypeCardinality::Scalar => then(this, expr, true),
                    TypeCardinality::Iterator => {
                        if this.ctx.late_types[this.ctx.term_decls[this.decl].ret_ty].cardinality
                            == TypeCardinality::Iterator
                        {
                            let ty = this
                                .ctx
                                .intern_type(Type::option(this.ctx.late_types[ty].ty));
                            let next = this.ctx.intern_expr(Expr {
                                kind: ExprKind::Next(expr),
                                ty,
                            });
                            let ty = this
                                .ctx
                                .intern_type(Type::scalar(this.ctx.late_types[ty].ty));
                            let item = this.ctx.intern_expr(Expr {
                                kind: ExprKind::MatchSome(next),
                                ty,
                            });

                            EvalStep {
                                bindings: Vec::new(),
                                ctrl_flow: ControlFlow::Loop {
                                    item,
                                    iterator: expr,
                                    body: Block(then(this, item, false).into()),
                                },
                            }
                            .into()
                        } else {
                            let ty = this
                                .ctx
                                .intern_type(Type::option(this.ctx.late_types[ty].ty));
                            let next = this.ctx.intern_expr(Expr {
                                kind: ExprKind::Next(expr),
                                ty,
                            });
                            let ty = this
                                .ctx
                                .intern_type(Type::scalar(this.ctx.late_types[ty].ty));
                            let some = this.ctx.intern_expr(Expr {
                                kind: ExprKind::MatchSome(next),
                                ty,
                            });

                            EvalStep {
                                bindings: Vec::new(),
                                ctrl_flow: ControlFlow::Match {
                                    target: next,
                                    arms: vec![MatchArm {
                                        constraint: Constraint::Some,
                                        bindings: vec![some],
                                        body: Block(then(this, some, false).into()),
                                    }],
                                },
                            }
                            .into()
                        }
                    }
                }
            }),
            sema::ExprKind::Global(global) => {
                let ty = self.ctx.intern_type(Type::scalar(expr.ty));
                let expr = self.ctx.intern_expr(Expr {
                    kind: ExprKind::Global(global),
                    ty,
                });

                then(self, expr, false)
            }
            sema::ExprKind::Literal(lit) => {
                let ty = self.ctx.intern_type(Type::scalar(expr.ty));
                let expr = self.ctx.intern_expr(Expr {
                    kind: ExprKind::Literal(lit),
                    ty,
                });

                then(self, expr, false)
            }
            sema::ExprKind::Tuple(exprs) => self.compile_exprs(exprs, |this, exprs| {
                let ty = this.ctx.intern_type(Type::scalar(expr.ty));
                let expr = this.ctx.intern_expr(Expr {
                    kind: ExprKind::Tuple(exprs),
                    ty,
                });

                then(this, expr, this.rows[0].is_bound(expr))
            }),
            sema::ExprKind::TupleIndex { expr: tuple, idx } => {
                self.compile_expr(*tuple, &mut |this, expr_idx, _| {
                    let ty = this.ctx.intern_type(Type::scalar(expr.ty));
                    let expr = this.ctx.intern_expr(Expr {
                        kind: ExprKind::TupleIndex {
                            expr: expr_idx,
                            idx,
                        },
                        ty,
                    });

                    then(this, expr, false)
                })
            }
            sema::ExprKind::Variable(var) => then(self, self.rows[0].variables[&var], false),
        }
    }

    fn swap(&mut self, idx: usize) {
        self.cols.swap(0, idx);

        for row in &mut self.rows {
            row.pats.swap(0, idx);
        }
    }

    fn unresolved_row(&self) -> Option<RowIdx> {
        self.rows
            .iter_enumerated()
            .rev()
            .find_map(|(idx, row)| (!row.is_resolved()).then_some(idx))
    }
}

struct CompilationContext<'a> {
    term_decls: IndexVec<TermDeclIdx, TermDecl>,
    sema_types: &'a IndexVec<sema::TypeIdx, sema::Type>,
    late_types: IndexVec<TypeIdx, Type>,
    late_type_dedup: HashMap<Type, TypeIdx>,
    exprs: IndexVec<ExprIdx, Expr>,
    expr_dedup: HashMap<Expr, ExprIdx>,
}

impl<'a> CompilationContext<'a> {
    fn new(program: &'a sema::Program) -> Self {
        Self {
            term_decls: IndexVec::new(),
            sema_types: &program.types,
            late_types: IndexVec::new(),
            late_type_dedup: HashMap::new(),
            exprs: IndexVec::new(),
            expr_dedup: HashMap::new(),
        }
    }

    fn lower_term_decls(&mut self, term_decls: IndexVec<TermDeclIdx, sema::TermDecl>) {
        self.term_decls = term_decls
            .into_iter()
            .map(|decl| {
                let ty = match decl.kind {
                    ConcreteTermKind::Partial => Type::option(decl.ret_ty),
                    ConcreteTermKind::Total => Type::scalar(decl.ret_ty),
                    ConcreteTermKind::Multi => Type::iterator(decl.ret_ty),
                };

                TermDecl {
                    name: decl.name,
                    arg_tys: decl.arg_tys,
                    ret_ty: self.intern_type(ty),
                    is_external: decl.is_external,
                    is_pure: decl.is_pure,
                }
            })
            .collect();
    }

    fn intern_expr(&mut self, expr: Expr) -> ExprIdx {
        // impure term calls & iterators can't be reused, so each instance get a
        // unique `ExprIdx`
        if matches!(expr.kind, ExprKind::Call { term_decl, .. } if !self.term_decls[term_decl].is_pure)
            || self.late_types[expr.ty].cardinality == TypeCardinality::Iterator
        {
            self.exprs.push(expr)
        } else {
            *self
                .expr_dedup
                .entry(expr)
                .or_insert_with_key(|expr| self.exprs.push(expr.clone()))
        }
    }

    fn intern_type(&mut self, ty: Type) -> TypeIdx {
        *self
            .late_type_dedup
            .entry(ty)
            .or_insert_with_key(|ty| self.late_types.push(ty.clone()))
    }

    fn compile(&mut self, decl: TermDeclIdx, rules: Vec<Rule>) -> Block {
        let steps = Compiler::new(self, decl, rules).compile();

        Block(steps)
    }
}

fn group_ctors(ctors: BTreeSet<Constructor>) -> Vec<Vec<Constructor>> {
    let mut groups = Vec::new();

    for ctor in ctors {
        if !ctor.is_groupable() {
            groups.push(vec![ctor]);

            continue;
        }

        match groups
            .iter_mut()
            // will I be executed by *the real* rust programmers for using
            // `std::mem::discriminant`?
            .find(|group| std::mem::discriminant(&group[0]) == std::mem::discriminant(&ctor))
        {
            Some(group) => group.push(ctor),
            None => groups.push(vec![ctor]),
        }
    }

    groups
}

struct SemaExprEnv<'a> {
    expr: &'a sema::Expr,
    variables: &'a HashMap<VariableIdx, ExprIdx>,
}

impl<'a> SemaExprEnv<'a> {
    fn new(expr: &'a sema::Expr, variables: &'a HashMap<VariableIdx, ExprIdx>) -> Self {
        Self { expr, variables }
    }

    fn sub(&self, expr: &'a sema::Expr) -> Self {
        Self {
            expr,
            variables: self.variables,
        }
    }

    fn all_deps_computed(&self) -> bool {
        match &self.expr.kind {
            sema::ExprKind::Call { args, .. } => {
                args.iter().all(|expr| self.sub(expr).all_deps_computed())
            }
            sema::ExprKind::Tuple(exprs) => {
                exprs.iter().all(|expr| self.sub(expr).all_deps_computed())
            }
            sema::ExprKind::TupleIndex { expr, .. } => self.sub(expr).all_deps_computed(),
            sema::ExprKind::Variable(var) => self.variables.contains_key(var),
            sema::ExprKind::Global(_) | sema::ExprKind::Literal(_) => true,
        }
    }
}

// holy fuck it looks ugly
impl PartialEq for SemaExprEnv<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.expr.ty != other.expr.ty {
            return false;
        }

        match (&self.expr.kind, &other.expr.kind) {
            (
                sema::ExprKind::Call {
                    term_decl: a_decl,
                    args: a_args,
                },
                sema::ExprKind::Call {
                    term_decl: b_decl,
                    args: b_args,
                },
            ) => {
                a_decl == b_decl
                    && a_args
                        .iter()
                        .zip(b_args)
                        .all(|(a, b)| self.sub(a) == other.sub(b))
            }
            (sema::ExprKind::Global(a), sema::ExprKind::Global(b)) => a == b,
            (sema::ExprKind::Literal(a), sema::ExprKind::Literal(b)) => a == b,
            (sema::ExprKind::Tuple(a), sema::ExprKind::Tuple(b)) => {
                a.iter().zip(b).all(|(a, b)| self.sub(a) == other.sub(b))
            }
            (
                sema::ExprKind::TupleIndex {
                    expr: a_expr,
                    idx: a_idx,
                },
                sema::ExprKind::TupleIndex {
                    expr: b_expr,
                    idx: b_idx,
                },
            ) => a_idx == b_idx && self.sub(a_expr) == other.sub(b_expr),
            (sema::ExprKind::Variable(a), sema::ExprKind::Variable(b)) => {
                self.variables[a] == other.variables[b]
            }
            _ => false,
        }
    }
}

pub(crate) fn run(mut program: sema::Program) -> Program {
    let rules = std::mem::take(&mut program.rules);
    let term_decls = std::mem::take(&mut program.term_decls);
    let mut ctx = CompilationContext::new(&program);

    ctx.lower_term_decls(term_decls);

    let terms = rules
        .into_iter()
        .map(|(decl, rules)| {
            let mut block = ctx.compile(decl, rules);

            if block.0.last().is_some_and(|step| {
                !matches!(step.ctrl_flow, ControlFlow::Return(_) | ControlFlow::Bail)
            }) || ctx.late_types[ctx.term_decls[decl].ret_ty].cardinality
                == TypeCardinality::Iterator
            {
                block.0.push(EvalStep {
                    bindings: Vec::new(),
                    ctrl_flow: ControlFlow::Bail,
                });
            }

            (decl, block)
        })
        .collect();

    Program {
        term_decls: ctx.term_decls,
        late_types: ctx.late_types,
        exprs: ctx.exprs,
        globals: program.globals,
        sema_types: program.types,
        terms,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{codegen::pseudo, grammar::DefinitionsParser, lexer::Lexer, sema};

    #[test]
    fn ctor_grouping() {
        assert_eq!(
            group_ctors(
                vec![Constructor::Int(0), Constructor::Int(1)]
                    .into_iter()
                    .collect()
            ),
            vec![vec![Constructor::Int(0), Constructor::Int(1)]],
        );

        assert_eq!(
            group_ctors(
                vec![
                    Constructor::Int(0),
                    Constructor::Int(1),
                    Constructor::Local(ExprIdx::from_usize(0)),
                    Constructor::Term(TermDeclIdx::from_usize(0))
                ]
                .into_iter()
                .collect()
            ),
            vec![
                vec![Constructor::Int(0), Constructor::Int(1)],
                vec![Constructor::Local(ExprIdx::from_usize(0))],
                vec![Constructor::Term(TermDeclIdx::from_usize(0))]
            ],
        );
    }

    macro_rules! expect {
        ($name: ident, $input: expr, $expected: expr) => {
            #[test]
            fn $name() {
                let defs = DefinitionsParser::new()
                    .parse(Lexer::new($input))
                    .expect("must parse");
                let program = sema::run(defs);
                let program = run(program);
                let mut buf = String::new();

                pseudo::generate(&program, &mut buf).expect("must be printed");

                assert_eq!(&buf, $expected.trim_start());
            }
        };
    }

    expect!(
        bindigns_erasure,
        r"
            pure partial term foo(bool) -> u8;
            extern partial term bar(bool) -> u8;

            rule foo(false) {
                let a: u8 = 0;
                let b: u8 = bar(true);
                let c: u8 = 3;

                b
            }
        ",
        r"
extern term @bar(bool) -> Option<u8>;

term foo(p0: bool) -> Option<u8> {
    match p0 {
        false => {
            match @bar(true) {
                Some(v0) => {
                    return Some(v0);
                }
            }
            bail;
        }
    }
    bail;
}
"
    );

    expect!(
        multiple_calls,
        r"
            partial term foo(bool) -> u8;
            extern pure partial term bar(bool) -> u8;
            extern partial term baz(u8) -> u8;

            rule foo(bar(a)) {
                let foo: u8 = baz(a);

                foo
            }
        ",
        r"
extern term @bar(bool) -> Option<u8>;
extern term @baz(u8) -> Option<u8>;

term foo(p0: bool) -> Option<u8> {
    match @bar(p0) {
        Some(v0) => {
            match @baz(v0) {
                Some(v1) => {
                    return Some(v1);
                }
            }
            bail;
        }
    }
    bail;
}
"
    );

    expect!(
        wildcard_pat,
        r"
            partial term foo(bool, u8) -> u8;
            extern pure partial term bar(bool) -> u8;

            rule foo(bar(a), a) {
                0
            }

            rule foo(_, _) {
                1
            }
        ",
        r"
extern term @bar(bool) -> Option<u8>;

term foo(p0: bool, p1: u8) -> Option<u8> {
    match @bar(p0) {
        Some(v0) => {
            match p1 {
                v0 => {
                    return Some(0);
                }
            }
        }
    }
    return Some(1);
}
"
    );

    expect!(
        multi_term_in_pat,
        r"
            partial term foo(bool) -> u8;
            extern pure multi term bar(bool) -> u8;

            rule foo(bar(a)) {
                a
            }
        ",
        r"
extern term @bar(bool) -> Iterator<u8>;

term foo(p0: bool) -> Option<u8> {
    for v0 in @bar(p0) {
        return Some(v0);
    }
    bail;
}
"
    );

    expect!(
        multi_term_in_body,
        r"
            partial term foo(bool) -> u8;
            extern pure multi term bar(bool) -> u8;

            rule foo(a) {
                bar(a)
            }
        ",
        r"
extern term @bar(bool) -> Iterator<u8>;

term foo(p0: bool) -> Option<u8> {
    match @bar(p0).next() {
        Some(v0) => {
            return Some(v0);
        }
    }
    bail;
}
"
    );

    expect!(
        multiple_impure_calls,
        r"
            partial term foo(bool) -> u8;
            extern total term bar(bool) -> u8;

            rule foo(a) {
                let b: u8 = bar(a);
                let c: u8 = bar(a);

                0
            }
        ",
        r"
extern term @bar(bool) -> u8;

term foo(p0: bool) -> Option<u8> {
    let v0: u8 = @bar(p0);
    let v1: u8 = @bar(p0);
    return Some(0);
}
"
    );

    expect!(
        bail_after_return_in_multi_term,
        r"
            multi term foo(bool) -> u8;

            rule foo(_) {
                0
            }
        ",
        r"
term foo(p0: bool) -> Iterator<u8> {
    yield 0;
    bail;
}
"
    );

    expect!(
        guards,
        r"
            multi term foo(bool) -> u8;

            rule foo(a)
                if let true = a && let false = a
            {
                0
            }
        ",
        r"
term foo(p0: bool) -> Iterator<u8> {
    match p0 {
        true => {
            match p0 {
                false => {
                    yield 0;
                }
            }
        }
    }
    bail;
}
"
    );

    // NOTE: ideally diagnostic messages should be emitted in such cases instead
    // of panics, but it works for now
    macro_rules! expect_panic {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            #[should_panic(expected = $expected)]
            fn $name() {
                let defs = DefinitionsParser::new()
                    .parse(Lexer::new($input))
                    .expect("must parse");
                let program = sema::run(defs);

                run(program);
            }
        };
    }

    expect_panic!(
        overlapping_rules,
        r"
            total term foo(bool) -> u8;

            rule foo(true) {
                0
            }

            rule foo(true) {
                1
            }
        ",
        "overlapping rules"
    );
}
