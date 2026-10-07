use crate::ast::{self, ConcreteTermKind as TermDeclKind, Definition, Extern, GlobalKind, Term};
use derive_more::Display;
use slotmap::{SlotMap, new_key_type};
use std::collections::{BTreeMap, HashMap, HashSet, hash_map::Entry};

pub(crate) struct Program {
    types: SlotMap<TypeId, Type>,
    globals: SlotMap<GlobalId, Global>,
    term_decls: SlotMap<TermDeclId, TermDecl>,
    rulesets: Vec<RuleSet>,
}

new_key_type! {
    struct TypeId;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
    Builtin(BuiltinType),
    External(String),
    Tuple(Vec<TypeId>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
enum BuiltinType {
    #[display("bool")]
    Bool,
    Int(IntType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
enum IntType {
    #[display("i8")]
    I8,
    #[display("i16")]
    I16,
    #[display("i32")]
    I32,
    #[display("i64")]
    I64,
    #[display("isize")]
    Isize,
    #[display("u8")]
    U8,
    #[display("u16")]
    U16,
    #[display("u32")]
    U32,
    #[display("u64")]
    U64,
    #[display("usize")]
    Usize,
}

impl From<ast::IntType> for IntType {
    fn from(ty: ast::IntType) -> Self {
        match ty {
            ast::IntType::I8 => Self::I8,
            ast::IntType::I16 => Self::I16,
            ast::IntType::I32 => Self::I32,
            ast::IntType::I64 => Self::I64,
            ast::IntType::Isize => Self::Isize,
            ast::IntType::U8 => Self::U8,
            ast::IntType::U16 => Self::U16,
            ast::IntType::U32 => Self::U32,
            ast::IntType::U64 => Self::U64,
            ast::IntType::Usize => Self::Usize,
        }
    }
}

struct DisplayType<'a> {
    ctx: &'a LoweringCtx,
    ty: TypeId,
}

impl<'a> DisplayType<'a> {
    fn new(ctx: &'a LoweringCtx, ty: TypeId) -> Self {
        Self { ctx, ty }
    }
}

impl std::fmt::Display for DisplayType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ctx.types[self.ty] {
            Type::Builtin(ty) => write!(f, "{}", ty),
            Type::External(name) => write!(f, "{}", name),
            Type::Tuple(tys) => {
                write!(f, "(")?;

                if let Some((&ty, tys)) = tys.split_first() {
                    write!(f, "{}", Self { ctx: self.ctx, ty })?;

                    for &ty in tys {
                        write!(f, ", {}", Self { ctx: self.ctx, ty })?;
                    }
                }

                write!(f, ")")?;

                Ok(())
            }
        }
    }
}

new_key_type! {
    struct GlobalId;
}

struct Global {
    name: String,
    ty: TypeId,
    kind: GlobalKind,
}

new_key_type! {
    struct TermDeclId;
}

struct TermDecl {
    kind: TermDeclKind,
    name: String,
    arg_tys: Vec<TypeId>,
    ret_ty: TypeId,
    is_external: bool,
    is_pure: bool,
}

new_key_type! {
    struct OverloadTermId;
}

struct OverloadTerm(Vec<TermDeclId>);

#[derive(Clone, Copy)]
enum TermSymbol {
    Concrete(TermDeclId),
    Overload(OverloadTermId),
}

struct RuleSet {
    term_decl: TermDeclId,
    rules: Vec<Rule>,
    exprs: SlotMap<ExprId, Expr>,
}

impl RuleSet {
    fn new(decl: TermDeclId) -> Self {
        Self {
            term_decl: decl,
            rules: Vec::new(),
            exprs: SlotMap::with_key(),
        }
    }
}

struct RuleSetLoweringCtx<'a> {
    ctx: &'a mut LoweringCtx,
    ruleset: RuleSet,
    expr_dedeup: HashMap<Expr, ExprId>,
}

impl<'a> RuleSetLoweringCtx<'a> {
    fn new(ctx: &'a mut LoweringCtx, decl: TermDeclId) -> Self {
        Self {
            ctx,
            ruleset: RuleSet::new(decl),
            expr_dedeup: HashMap::new(),
        }
    }

    fn intern_expr(&mut self, expr: Expr) -> ExprId {
        match expr.kind {
            ExprKind::Call { term_decl, .. } if !self.ctx.term_decls[term_decl].is_pure => {
                self.ruleset.exprs.insert(expr)
            }
            _ => *self
                .expr_dedeup
                .entry(expr)
                .or_insert_with_key(|expr| self.ruleset.exprs.insert(expr.clone())),
        }
    }

    fn lower(mut self, rules: Vec<(&[ast::Pattern], &ast::Rule)>) -> RuleSet {
        self.ruleset.rules = rules
            .iter()
            .map(|(pats, rule)| RuleLoweringCtx::new(&mut self).lower(pats, rule))
            .collect();

        self.ruleset
    }
}

struct Rule {
    pats: Vec<Pattern>,
    guards: Vec<Guard>,
    body: Body,
}

#[derive(Clone)]
enum ExpectedType {
    OneOf(HashSet<TypeId>),
    Any,
}

#[derive(Clone, Copy, PartialEq)]
enum ExprCtx {
    Guard,
    Body,
}

struct RuleLoweringCtx<'a, 'b> {
    ruleset_ctx: &'b mut RuleSetLoweringCtx<'a>,
    bindings: HashMap<&'b str, ExprId>,
}

impl<'a, 'b> RuleLoweringCtx<'a, 'b> {
    fn new(ruleset_ctx: &'b mut RuleSetLoweringCtx<'a>) -> Self {
        Self {
            ruleset_ctx,
            bindings: HashMap::new(),
        }
    }

    fn display_type<'s>(&'s self, ty: TypeId) -> DisplayType<'s> {
        DisplayType::new(self.ruleset_ctx.ctx, ty)
    }

    fn lower_pat(&mut self, pat: &'b ast::Pattern, expr: ExprId) -> Pattern {
        let ty = self.ruleset_ctx.ruleset.exprs[expr].ty;

        match pat {
            ast::Pattern::Application { name, args } => {
                let Some(&symbol) = self.ruleset_ctx.ctx.term_symbols.get(name) else {
                    panic!("term '{}' not defined", name);
                };
                let decl = match symbol {
                    TermSymbol::Concrete(decl) => {
                        let arg_tys = &self.ruleset_ctx.ctx.term_decls[decl].arg_tys;

                        if arg_tys != &[ty] {
                            panic!(
                                "bad pattern term: expected a term with 1 parameter of type '{}', but it takes {} instead",
                                self.display_type(ty),
                                arg_tys
                                    .iter()
                                    .map(|&ty| format!("'{}'", self.display_type(ty).to_string()))
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            );
                        }

                        decl
                    }
                    TermSymbol::Overload(term) => *self.ruleset_ctx.ctx.overload_terms[term]
                        .0
                        .iter()
                        .find(|&&decl| self.ruleset_ctx.ctx.term_decls[decl].arg_tys == &[ty])
                        .expect("no matching overload found"),
                };
                let expr = self.ruleset_ctx.intern_expr(Expr {
                    kind: ExprKind::Call {
                        term_decl: decl,
                        args: vec![expr],
                    },
                    ty: self.ruleset_ctx.ctx.term_decls[decl].ret_ty,
                });
                let exprs = match &self.ruleset_ctx.ctx.types
                    [self.ruleset_ctx.ctx.term_decls[decl].ret_ty]
                    .clone()
                {
                    Type::Tuple(tys) => tys
                        .iter()
                        .enumerate()
                        .map(|(idx, &ty)| {
                            self.ruleset_ctx.intern_expr(Expr {
                                kind: ExprKind::TupleIndex { expr, idx },
                                ty,
                            })
                        })
                        .collect(),
                    _ => vec![expr],
                };

                assert_eq!(
                    exprs.len(),
                    args.len(),
                    "term arity mismatch: expected {}, got {}",
                    exprs.len(),
                    args.len(),
                );

                let mut pats = Vec::with_capacity(exprs.len());

                for (pat, expr) in args.iter().zip(exprs) {
                    pats.push(self.lower_pat(pat, expr));
                }

                Pattern::Application {
                    term_decl: decl,
                    pats,
                }
            }
            ast::Pattern::Tuple(pats) => {
                let Type::Tuple(tys) = &self.ruleset_ctx.ctx.types[ty] else {
                    panic!(
                        "pattern type mismatch: expected '{}', found tuple",
                        self.display_type(ty),
                    );
                };

                assert_eq!(
                    tys.len(),
                    pats.len(),
                    "tuple pattern arity mismatch: expected {}, got {}",
                    tys.len(),
                    pats.len()
                );

                let mut lowered_pats = Vec::with_capacity(pats.len());

                for (idx, (pat, ty)) in pats.iter().zip(tys.clone()).enumerate() {
                    let expr = self.ruleset_ctx.intern_expr(Expr {
                        kind: ExprKind::TupleIndex { expr, idx },
                        ty,
                    });

                    lowered_pats.push(self.lower_pat(pat, expr));
                }

                Pattern::Tuple(lowered_pats)
            }
            &ast::Pattern::Literal(ast::Literal::Int { value, ty: int_ty }) => {
                let Type::Builtin(BuiltinType::Int(_)) = self.ruleset_ctx.ctx.types[ty] else {
                    if let Some(int_ty) = int_ty {
                        let pat_ty = self
                            .ruleset_ctx
                            .ctx
                            .intern_type(Type::Builtin(BuiltinType::Int(int_ty.into())));

                        assert_eq!(
                            ty,
                            pat_ty,
                            "pattern type mismatch: expected '{}', found '{}'",
                            self.display_type(ty),
                            self.display_type(pat_ty),
                        );
                    }

                    panic!(
                        "pattern type mismatch: expected '{}', found integer",
                        self.display_type(ty),
                    );
                };

                Pattern::Literal(Literal::Int(value))
            }
            &ast::Pattern::Literal(ast::Literal::Bool(value)) => {
                assert_eq!(
                    self.ruleset_ctx.ctx.types[ty],
                    Type::Builtin(BuiltinType::Bool),
                    "pattern type mismatch: expected '{}', found 'bool'",
                    self.display_type(ty),
                );

                Pattern::Literal(Literal::Bool(value))
            }
            ast::Pattern::Ident(ident) => {
                let pat_match = match self.ruleset_ctx.ctx.global_symbols.get(ident) {
                    Some(&global) => Some(Match::Global(global)),
                    None => match self.bindings.entry(ident) {
                        Entry::Occupied(entry) => Some(Match::Expr(*entry.get())),
                        Entry::Vacant(entry) => {
                            entry.insert(expr);

                            None
                        }
                    },
                };

                match pat_match {
                    Some(pat_match) => {
                        let pat_ty = match pat_match {
                            Match::Expr(expr) => self.ruleset_ctx.ruleset.exprs[expr].ty,
                            Match::Global(global) => self.ruleset_ctx.ctx.globals[global].ty,
                        };

                        assert_eq!(
                            ty,
                            pat_ty,
                            "pattern type mismatch: expected '{}', found '{}'",
                            self.display_type(ty),
                            self.display_type(pat_ty)
                        );

                        Pattern::Match(pat_match)
                    }
                    None => Pattern::Wildcard,
                }
            }
            ast::Pattern::Wildcard => Pattern::Wildcard,
        }
    }

    fn lower_expr_impl(
        &mut self,
        expr: &ast::Expr,
        expected_ty: ExpectedType,
        ctx: ExprCtx,
    ) -> ExprId {
        let expr = match expr {
            ast::Expr::Call { name, args } => {
                let Some(&symbol) = self.ruleset_ctx.ctx.term_symbols.get(name) else {
                    panic!("term '{}' not defined", name);
                };
                let (decl, args) = match symbol {
                    TermSymbol::Concrete(decl_id) => {
                        let decl = &self.ruleset_ctx.ctx.term_decls[decl_id];

                        assert_eq!(
                            decl.arg_tys.len(),
                            args.len(),
                            "expected {} arguments, found {}",
                            decl.arg_tys.len(),
                            args.len()
                        );

                        let args = args
                            .iter()
                            .zip(decl.arg_tys.clone())
                            .map(|(expr, ty)| self.lower_expr(expr, Some(ty), ctx))
                            .collect();

                        (decl_id, args)
                    }
                    TermSymbol::Overload(overload) => {
                        let decls = self.ruleset_ctx.ctx.overload_terms[overload].0.clone();
                        let compute_expected_ty =
                            |this: &mut RuleLoweringCtx, needle: &[TypeId]| {
                                ExpectedType::OneOf(
                                    decls
                                        .iter()
                                        .filter_map(|&decl| {
                                            let tys =
                                                &this.ruleset_ctx.ctx.term_decls[decl].arg_tys;

                                            if tys.len() == args.len() && tys.starts_with(needle) {
                                                Some(tys[needle.len()])
                                            } else {
                                                None
                                            }
                                        })
                                        .collect(),
                                )
                            };
                        let mut tys = Vec::with_capacity(args.len());
                        let mut exprs = Vec::with_capacity(args.len());

                        for arg in args {
                            let expected_type = compute_expected_ty(self, &tys);
                            let expr = self.lower_expr_impl(arg, expected_type, ctx);

                            tys.push(self.ruleset_ctx.ruleset.exprs[expr].ty);
                            exprs.push(expr);
                        }

                        let decl = decls
                            .into_iter()
                            .find(|&decl| self.ruleset_ctx.ctx.term_decls[decl].arg_tys == tys)
                            .unwrap();

                        (decl, exprs)
                    }
                };
                let decls = &self.ruleset_ctx.ctx.term_decls;

                if ctx == ExprCtx::Body
                    && decls[self.ruleset_ctx.ruleset.term_decl].kind == TermDeclKind::Total
                    && matches!(
                        decls[decl].kind,
                        TermDeclKind::Partial | TermDeclKind::Multi
                    )
                {
                    panic!(
                        "term '{}' can't be used in total term rule '{}'; total term rules must use only total terms in body",
                        decls[decl].name, decls[self.ruleset_ctx.ruleset.term_decl].name
                    );
                }

                if ctx == ExprCtx::Guard && !decls[decl].is_pure {
                    panic!(
                        "term '{}' can't be used in guard expression; guards must use pure terms",
                        decls[decl].name
                    );
                }

                self.ruleset_ctx.intern_expr(Expr {
                    kind: ExprKind::Call {
                        term_decl: decl,
                        args,
                    },
                    ty: decls[decl].ret_ty,
                })
            }
            ast::Expr::Tuple(exprs) => {
                let compute_expected_ty =
                    |this: &mut RuleLoweringCtx, needle: &[TypeId]| match &expected_ty {
                        ExpectedType::OneOf(tys) => ExpectedType::OneOf(
                            tys.iter()
                                .filter_map(|&ty| match &this.ruleset_ctx.ctx.types[ty] {
                                    Type::Tuple(tys)
                                        if exprs.len() == tys.len() && tys.starts_with(needle) =>
                                    {
                                        Some(tys[needle.len()])
                                    }
                                    _ => None,
                                })
                                .collect(),
                        ),
                        ExpectedType::Any => ExpectedType::Any,
                    };
                let mut tys = Vec::with_capacity(exprs.len());
                let mut lowered_exprs = Vec::with_capacity(exprs.len());

                for expr in exprs {
                    let expected_ty = compute_expected_ty(self, &tys);
                    let expr = self.lower_expr_impl(expr, expected_ty, ctx);

                    tys.push(self.ruleset_ctx.ruleset.exprs[expr].ty);
                    lowered_exprs.push(expr);
                }

                let ty = self.ruleset_ctx.ctx.intern_type(Type::Tuple(tys));

                self.ruleset_ctx.intern_expr(Expr {
                    kind: ExprKind::Tuple(lowered_exprs),
                    ty,
                })
            }
            &ast::Expr::TupleIndex { ref expr, idx } => {
                let expr = self.lower_expr_impl(expr, ExpectedType::Any, ctx);
                let ty = self.ruleset_ctx.ruleset.exprs[expr].ty;
                let ty = match &self.ruleset_ctx.ctx.types[ty] {
                    Type::Tuple(tys) => *tys.get(idx).unwrap_or_else(|| {
                        panic!(
                            "tuple index {} is greater than number of elements in tuple type '{}'",
                            idx,
                            self.display_type(ty)
                        )
                    }),
                    _ => panic!(
                        "type mismatch: expected tuple, found '{}'",
                        self.display_type(ty)
                    ),
                };

                self.ruleset_ctx.intern_expr(Expr {
                    kind: ExprKind::TupleIndex { expr, idx },
                    ty,
                })
            }
            &ast::Expr::Literal(ast::Literal::Int { value, ty }) => {
                let expected_ty = match expected_ty {
                    ExpectedType::OneOf(tys) => {
                        match tys
                            .iter()
                            .filter_map(|&ty| match self.ruleset_ctx.ctx.types[ty] {
                                Type::Builtin(BuiltinType::Int(_)) => Some(ty),
                                _ => None,
                            })
                            .collect::<Vec<_>>()
                            .as_slice()
                        {
                            [] => None,
                            &[ty] => Some(ty),
                            _ => panic!("ambiguous integer value"),
                        }
                    }
                    ExpectedType::Any => None,
                };
                let provided_ty = ty.map(|int_ty| {
                    self.ruleset_ctx
                        .ctx
                        .intern_type(Type::Builtin(BuiltinType::Int(int_ty.into())))
                });
                let ty = match (expected_ty, provided_ty) {
                    (Some(ty), None) | (None, Some(ty)) => ty,
                    (Some(expected), Some(provided)) => {
                        assert_eq!(
                            expected,
                            provided,
                            "type mismatch: expected '{}', found '{}'",
                            self.display_type(expected),
                            self.display_type(provided)
                        );

                        expected
                    }
                    (None, None) => {
                        panic!("failed to infer integer's type");
                    }
                };

                return self.ruleset_ctx.intern_expr(Expr {
                    kind: ExprKind::Literal(Literal::Int(value)),
                    ty,
                });
            }
            &ast::Expr::Literal(ast::Literal::Bool(value)) => {
                let ty = self
                    .ruleset_ctx
                    .ctx
                    .intern_type(Type::Builtin(BuiltinType::Bool));

                self.ruleset_ctx.intern_expr(Expr {
                    kind: ExprKind::Literal(Literal::Bool(value)),
                    ty,
                })
            }
            ast::Expr::Ident(ident) => match self.ruleset_ctx.ctx.global_symbols.get(ident) {
                Some(&global) => self.ruleset_ctx.intern_expr(Expr {
                    kind: ExprKind::Global(global),
                    ty: self.ruleset_ctx.ctx.globals[global].ty,
                }),
                None => *self
                    .bindings
                    .get(ident.as_str())
                    .unwrap_or_else(|| panic!("binding '{}' not found", ident)),
            },
        };

        if let ExpectedType::OneOf(tys) = expected_ty {
            let ty = self.ruleset_ctx.ruleset.exprs[expr].ty;
            let mut tys = tys.into_iter().collect::<Vec<_>>();

            tys.sort();

            match tys.as_slice() {
                [] => unreachable!(),
                &[expected_ty] => {
                    assert_eq!(
                        expected_ty,
                        ty,
                        "type mismatch: expected '{}', found '{}",
                        self.display_type(expected_ty),
                        self.display_type(ty),
                    );
                }
                _ => {
                    assert!(
                        tys.contains(&ty),
                        "type mismatch: expected one of {}, found '{}'",
                        tys.into_iter()
                            .map(|ty| format!("'{}'", self.display_type(ty).to_string()))
                            .collect::<Vec<_>>()
                            .join(", "),
                        self.display_type(ty),
                    );
                }
            }
        }

        expr
    }

    fn lower_expr(
        &mut self,
        expr: &ast::Expr,
        expected_ty: Option<TypeId>,
        ctx: ExprCtx,
    ) -> ExprId {
        self.lower_expr_impl(
            expr,
            expected_ty
                .map(|ty| ExpectedType::OneOf(HashSet::from([ty])))
                .unwrap_or(ExpectedType::Any),
            ctx,
        )
    }

    fn lower(mut self, pats: &'b [ast::Pattern], rule: &'b ast::Rule) -> Rule {
        let decl = &self.ruleset_ctx.ctx.term_decls[self.ruleset_ctx.ruleset.term_decl];
        let ret_ty = decl.ret_ty;

        assert_eq!(
            decl.arg_tys.len(),
            pats.len(),
            "root rule pattern arity mismatch: expected {}, got {}",
            decl.arg_tys.len(),
            pats.len()
        );

        let mut lowered_pats = Vec::with_capacity(pats.len());

        for (idx, (pat, ty)) in pats.iter().zip(decl.arg_tys.clone()).enumerate() {
            let expr = self.ruleset_ctx.intern_expr(Expr {
                kind: ExprKind::Parameter(idx),
                ty,
            });

            lowered_pats.push(self.lower_pat(pat, expr));
        }

        let guards = rule
            .guards
            .iter()
            .map(|ast::Guard { pat, expr }| {
                let expr = self.lower_expr(expr, None, ExprCtx::Guard);
                let pat = self.lower_pat(pat, expr);

                Guard { pat, expr }
            })
            .collect();
        let body = {
            let bindings = rule
                .body
                .lets
                .iter()
                .map(|ast::Let { name, ty, value }| {
                    let ty = self.ruleset_ctx.ctx.lower_type(ty);
                    let expr = self.lower_expr(value, Some(ty), ExprCtx::Body);

                    assert!(
                        self.bindings.insert(name, expr).is_none(),
                        "redefinition of symbol '{}'",
                        name
                    );

                    expr
                })
                .collect();
            let result = self.lower_expr(&rule.body.expr, Some(ret_ty), ExprCtx::Body);

            Body { bindings, result }
        };

        Rule {
            pats: lowered_pats,
            guards,
            body,
        }
    }
}

enum Pattern {
    Application {
        term_decl: TermDeclId,
        pats: Vec<Self>,
    },
    Tuple(Vec<Self>),
    Literal(Literal),
    Match(Match),
    Wildcard,
}

enum Match {
    Expr(ExprId),
    Global(GlobalId),
}

new_key_type! {
    struct ExprId;
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct Expr {
    kind: ExprKind,
    ty: TypeId,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum ExprKind {
    Parameter(usize),
    Global(GlobalId),
    Tuple(Vec<ExprId>),
    Literal(Literal),
    TupleIndex {
        expr: ExprId,
        idx: usize,
    },
    Call {
        term_decl: TermDeclId,
        args: Vec<ExprId>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum Literal {
    Int(i64),
    Bool(bool),
}

struct Guard {
    pat: Pattern,
    expr: ExprId,
}

struct Body {
    bindings: Vec<ExprId>,
    result: ExprId,
}

pub(crate) struct LoweringCtx {
    types: SlotMap<TypeId, Type>,
    type_dedup: HashMap<Type, TypeId>,
    type_symbols: HashMap<String, TypeId>,

    globals: SlotMap<GlobalId, Global>,
    global_symbols: HashMap<String, GlobalId>,

    term_decls: SlotMap<TermDeclId, TermDecl>,
    overload_terms: SlotMap<OverloadTermId, OverloadTerm>,
    term_symbols: HashMap<String, TermSymbol>,

    rulesets: Vec<RuleSet>,
}

impl LoweringCtx {
    pub(crate) fn new() -> Self {
        let predefined = [
            ("bool", BuiltinType::Bool),
            ("i8", BuiltinType::Int(IntType::I8)),
            ("i16", BuiltinType::Int(IntType::I16)),
            ("i32", BuiltinType::Int(IntType::I32)),
            ("i64", BuiltinType::Int(IntType::I64)),
            ("isize", BuiltinType::Int(IntType::Isize)),
            ("u8", BuiltinType::Int(IntType::U8)),
            ("u16", BuiltinType::Int(IntType::U16)),
            ("u32", BuiltinType::Int(IntType::U32)),
            ("u64", BuiltinType::Int(IntType::U64)),
            ("usize", BuiltinType::Int(IntType::Usize)),
        ];
        let mut types = SlotMap::with_capacity_and_key(predefined.len());
        let mut type_dedup = HashMap::with_capacity(predefined.len());
        let mut type_symbols = HashMap::with_capacity(predefined.len());

        for (name, ty) in predefined {
            let ty = Type::Builtin(ty);
            let id = types.insert(ty.clone());

            type_dedup.insert(ty, id);
            type_symbols.insert(name.into(), id);
        }

        Self {
            types,
            type_dedup,
            type_symbols,

            globals: SlotMap::with_key(),
            global_symbols: HashMap::new(),

            term_decls: SlotMap::with_key(),
            overload_terms: SlotMap::with_key(),
            term_symbols: HashMap::new(),

            rulesets: Vec::new(),
        }
    }

    fn lower_types(&mut self, defs: &[Definition]) {
        for def in defs {
            match def {
                Definition::Extern(Extern::Type {
                    name,
                    external_name,
                }) => {
                    self.types.insert_with_key(|id| {
                        assert!(
                            self.type_symbols.insert(name.clone(), id).is_none(),
                            "redefinition of type '{name}'",
                        );

                        Type::External(external_name.as_ref().unwrap_or(name).clone())
                    });
                }
                _ => (),
            }
        }
    }

    fn lower_globals(&mut self, defs: &[Definition]) {
        for def in defs {
            match def {
                Definition::Extern(Extern::Global {
                    name,
                    ty,
                    kind,
                    external_name,
                }) => {
                    let global = Global {
                        name: external_name.clone(),
                        ty: self.lower_type(ty),
                        kind: *kind,
                    };

                    self.globals.insert_with_key(|id| {
                        assert!(
                            self.global_symbols.insert(name.clone(), id).is_none(),
                            "redefinition of global '{name}'",
                        );

                        global
                    });
                }
                _ => (),
            }
        }
    }

    fn intern_type(&mut self, ty: Type) -> TypeId {
        *self
            .type_dedup
            .entry(ty)
            .or_insert_with_key(|ty| self.types.insert(ty.clone()))
    }

    fn lower_type(&mut self, ty: &ast::Type) -> TypeId {
        match ty {
            ast::Type::Tuple(types) => {
                let ty = Type::Tuple(types.iter().map(|ty| self.lower_type(ty)).collect());

                self.intern_type(ty)
            }
            ast::Type::Ident(ident) => *self
                .type_symbols
                .get(ident)
                .unwrap_or_else(|| panic!("type '{}' not defined", ident)),
        }
    }

    fn lower_concrete_terms(&mut self, defs: &[Definition]) {
        for def in defs {
            let (external_name, term) = match def {
                Definition::Extern(Extern::Term {
                    term,
                    external_name,
                }) => (
                    Some(external_name.as_ref().unwrap_or(&term.name).clone()),
                    term,
                ),
                Definition::Term(Term::Concrete(term)) => (None, term),
                _ => continue,
            };
            let decl = TermDecl {
                kind: term.kind,
                is_external: external_name.is_some(),
                name: external_name.unwrap_or(term.name.clone()),
                arg_tys: term.arg_tys.iter().map(|ty| self.lower_type(ty)).collect(),
                ret_ty: self.lower_type(&term.ret_ty),
                is_pure: term.is_pure,
            };

            self.term_decls.insert_with_key(|id| {
                assert!(
                    self.term_symbols
                        .insert(term.name.clone(), TermSymbol::Concrete(id))
                        .is_none(),
                    "redefinition of term '{}'",
                    term.name
                );

                decl
            });
        }
    }

    fn lower_overload_terms(&mut self, defs: &[Definition]) {
        for def in defs {
            match def {
                Definition::Term(Term::Overload(term)) => {
                    let mut signatures = HashMap::new();
                    let mut decls = Vec::new();

                    for term_name in &term.terms {
                        let Some(symbol) = self.term_symbols.get(term_name) else {
                            panic!("term '{}' not defined", term_name);
                        };
                        let decl = match symbol {
                            &TermSymbol::Concrete(decl) => decl,
                            TermSymbol::Overload(_) => {
                                panic!(
                                    "overload term '{}' can't be overladed with overload term '{}'; only concrete terms are allowed",
                                    term.name, term_name
                                );
                            }
                        };

                        match signatures.insert(&self.term_decls[decl].arg_tys, term_name) {
                            Some(sig_term_name) => {
                                panic!(
                                    "overload term '{}' can't be overladed with term '{}'; parameter type list must be unique, but it's already covered by '{}'",
                                    term.name, term_name, sig_term_name
                                );
                            }
                            None => {
                                decls.push(decl);
                            }
                        }
                    }

                    self.overload_terms.insert_with_key(|id| {
                        assert!(
                            self.term_symbols
                                .insert(term.name.clone(), TermSymbol::Overload(id))
                                .is_none(),
                            "redefinition of overload term '{}'",
                            term.name,
                        );

                        OverloadTerm(decls)
                    });
                }
                _ => (),
            }
        }
    }

    fn lower_rules(&mut self, defs: &[Definition]) {
        let mut rules: BTreeMap<TermDeclId, Vec<(&[ast::Pattern], &ast::Rule)>> = BTreeMap::new();

        for def in defs {
            match def {
                Definition::Rule(rule) => {
                    let (term, pats) = match &rule.pat {
                        ast::Pattern::Application { name, args } => (name, args),
                        _ => panic!("rule's root pattern must be application"),
                    };
                    let decl = match self.term_symbols.get(term) {
                        Some(&TermSymbol::Concrete(decl)) => decl,
                        Some(TermSymbol::Overload(_)) => {
                            panic!("rule aren't allowed for overload terms");
                        }
                        None => panic!("term '{}' not defined", term),
                    };

                    rules.entry(decl).or_default().push((pats, rule));
                }
                _ => (),
            }
        }

        self.rulesets = rules
            .into_iter()
            .map(|(decl, rules)| RuleSetLoweringCtx::new(self, decl).lower(rules))
            .collect();
    }

    pub(crate) fn lower(mut self, defs: Vec<Definition>) -> Program {
        self.lower_types(&defs);
        self.lower_globals(&defs);
        self.lower_concrete_terms(&defs);
        self.lower_overload_terms(&defs);
        self.lower_rules(&defs);

        Program {
            types: self.types,
            globals: self.globals,
            term_decls: self.term_decls,
            rulesets: self.rulesets,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{grammar::DefinitionsParser, lexer::Lexer};

    // NOTE: ideally diagnostic messages should be emmited in such cases instead
    // of panics, but it works for now
    macro_rules! expect_panic {
        ($name: ident, $input: expr, $expected: expr) => {
            #[test]
            #[should_panic(expected = $expected)]
            fn $name() {
                let defs = DefinitionsParser::new()
                    .parse(Lexer::new($input))
                    .expect("must parse");

                LoweringCtx::new().lower(defs);
            }
        };
    }

    expect_panic!(
        type_redefinition,
        r"
            extern type Foo = Bar;
            extern type Foo = Baz;
        ",
        "redefinition of type 'Foo'"
    );

    expect_panic!(
        undefined_type,
        r"
            multi term foo() -> Foo;
        ",
        "type 'Foo' not defined"
    );

    expect_panic!(
        global_redefinition,
        r"
            extern let FOO: () = const FOO;
            extern let FOO: () = field bar;
        ",
        "redefinition of global 'FOO'"
    );

    expect_panic!(
        concrete_term_redefinition,
        r"
            partial term foo() -> ();
            multi term foo() -> ();
        ",
        "redefinition of term 'foo'"
    );

    expect_panic!(
        undefined_term_in_overload,
        r"
            overload term foo = bar;
        ",
        "term 'bar' not defined"
    );

    expect_panic!(
        overload_with_overload_term,
        r"
            extern type Foo;
            extern type Bar;
            extern type Baz;

            partial term foo(Foo) -> ();
            partial term bar(Bar) -> ();
            partial term baz(Baz) -> ();

            overload term qux = foo + bar;
            overload term quux = baz + qux;
        ",
        "overload term 'quux' can't be overladed with overload term 'qux'; only concrete terms are allowed"
    );

    expect_panic!(
        overload_overlap,
        r"
            extern type Foo;

            partial term foo1(Foo) -> ();
            partial term foo2(Foo) -> ();

            overload term bar = foo1 + foo2;
        ",
        "overload term 'bar' can't be overladed with term 'foo2'; parameter type list must be unique, but it's already covered by 'foo1'"
    );

    expect_panic!(
        overload_term_redefinition,
        r"
            extern type Foo;
            extern type Bar;

            partial term foo(Foo) -> ();
            partial term bar(Bar) -> ();

            overload term baz = foo + bar;
            overload term baz = foo + bar;
        ",
        "redefinition of overload term 'baz'"
    );

    expect_panic!(
        non_application_root_pat,
        r"
            rule 2 {
                ()
            }
        ",
        "rule's root pattern must be application"
    );

    expect_panic!(
        overload_term_rule,
        r"
            extern type Foo;

            partial term foo(Foo) -> ();
            partial term bar() -> ();
            overload term baz = foo + bar;

            rule baz(_) {
                ()
            }
        ",
        "rule aren't allowed for overload terms"
    );

    expect_panic!(
        undefined_term_rule,
        r"
            rule foo() {
                ()
            }
        ",
        "term 'foo' not defined"
    );

    expect_panic!(
        root_application_pat_arity_mismatch,
        r"
            partial term foo(usize) -> ();

            rule foo(69, 420) {
                ()
            }
        ",
        "root rule pattern arity mismatch: expected 1, got 2"
    );

    expect_panic!(
        undefined_term_application,
        r"
            partial term foo(usize) -> ();

            rule foo(bar(_)) {
                ()
            }
        ",
        "term 'bar' not defined"
    );

    expect_panic!(
        bad_term_application_pat,
        r"
            partial term foo(usize) -> ();
            partial term bar(usize, i32) -> u8;

            rule foo(bar(_)) {
                ()
            }
        ",
        "bad pattern term: expected a term with 1 parameter of type 'usize', but it takes 'usize', 'i32' instead"
    );

    expect_panic!(
        overload_term_application_pat_without_matching_overload,
        r"
            partial term foo(usize) -> ();
            partial term bar() -> ();

            overload term baz = bar;

            rule foo(baz(_)) {
                ()
            }
        ",
        "no matching overload found"
    );

    expect_panic!(
        application_pat_arity_mismatch,
        r"
            partial term foo(usize) -> ();
            partial term bar(usize) -> (usize, i8);

            rule foo(bar(_, _, _)) {
                ()
            }
        ",
        "term arity mismatch: expected 2, got 3"
    );

    expect_panic!(
        unexpected_tuple_pat,
        r"
            partial term foo(usize) -> ();

            rule foo((50)) {
                ()
            }
        ",
        "pattern type mismatch: expected 'usize', found tuple"
    );

    expect_panic!(
        tuple_pat_arity_mismatch,
        r"
            partial term foo((usize, i8)) -> ();

            rule foo((69)) {
                ()
            }
        ",
        "tuple pattern arity mismatch: expected 2, got 1"
    );

    expect_panic!(
        pat_type_mismatch_untyped_int,
        r"
            extern type Ty;

            partial term foo(Ty) -> ();

            rule foo(69) {
                ()
            }
        ",
        "pattern type mismatch: expected 'Ty', found integer"
    );

    expect_panic!(
        pat_type_mismatch_typed_int,
        r"
            extern type Ty;

            partial term foo(Ty) -> ();

            rule foo(69u8) {
                ()
            }
        ",
        "pattern type mismatch: expected 'Ty', found 'u8'"
    );

    expect_panic!(
        pat_unexpected_bool,
        r"
            partial term foo(i64) -> ();

            rule foo(true) {
                ()
            }
        ",
        "pattern type mismatch: expected 'i64', found 'bool'"
    );

    expect_panic!(
        reused_binding_type_mismatch,
        r"
            total term foo(usize, i8) -> ();

            rule foo(val, val) {
                ()
            }
        ",
        "pattern type mismatch: expected 'i8', found 'usize'"
    );

    expect_panic!(
        undefined_term_call,
        r"
            total term foo(usize) -> ();

            rule foo(69) {
                bar()
            }
        ",
        "term 'bar' not defined"
    );

    expect_panic!(
        call_args_number_mismatch,
        r"
            total term foo(usize) -> ();
            total term bar(u8, i16) -> ();

            rule foo(69) {
                bar(420, 520, 99)
            }
        ",
        "expected 2 arguments, found 3"
    );

    expect_panic!(
        non_total_term_call_in_total_term_rule,
        r"
            total term foo(usize) -> ();
            partial term bar() -> ();

            rule foo(69) {
                bar()
            }
        ",
        "term 'bar' can't be used in total term rule 'foo'; total term rules must use only total terms in body"
    );

    expect_panic!(
        impure_guard,
        r"
            total term foo(usize) -> ();
            partial term bar() -> ();

            rule foo(69)
                if let _ = bar()
            {
                ()
            }
        ",
        "term 'bar' can't be used in guard expression; guards must use pure terms"
    );

    expect_panic!(
        tuple_index_out_of_bounds,
        r"
            total term foo(usize) -> ();

            rule foo(69) {
                (()).1
            }
        ",
        "tuple index 1 is greater than number of elements in tuple type '(())'"
    );

    expect_panic!(
        tuple_index_of_non_tuple_type,
        r"
            total term foo(usize) -> ();
            total term bar() -> u8;

            rule foo(69) {
                bar().1
            }
        ",
        "type mismatch: expected tuple, found 'u8'"
    );

    expect_panic!(
        ambiguous_int_expr,
        r"
            total term foo(usize) -> ();
            total term t1(u8) -> ();
            total term t2(usize) -> ();

            overload term t = t1 + t2;

            rule foo(69) {
                t(92)
            }
        ",
        "ambiguous integer value"
    );

    expect_panic!(
        int_type_mismatch,
        r"
            total term foo(usize) -> ();
            total term bar(u8) -> ();

            rule foo(69) {
                bar(0u16)
            }
        ",
        "type mismatch: expected 'u8', found 'u16'"
    );

    expect_panic!(
        int_type_infer_fail,
        r"
            extern type Type1;
            extern type Type2;

            total term foo(usize) -> ();
            total term t1(Type1) -> ();
            total term t2(Type2) -> ();

            overload term t = t1 + t2;

            rule foo(69) {
                t(92)
            }
        ",
        "failed to infer integer's type"
    );

    expect_panic!(
        udefined_binding,
        r"
            total term foo(usize) -> ();

            rule foo(69) {
                bar
            }
        ",
        "binding 'bar' not found"
    );

    expect_panic!(
        one_of_type_mismatch,
        r"
            extern type Type1;
            extern type Type2;

            total term foo(usize) -> ();
            total term t1(Type1) -> ();
            total term t2(Type2) -> ();

            overload term t = t1 + t2;

            rule foo(69) {
                t(true)
            }
        ",
        "type mismatch: expected one of 'Type1', 'Type2', found 'bool'"
    );
}
