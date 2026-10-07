use crate::{
    TermDeclIdx,
    ast::{self, ConcreteTermKind as TermDeclKind, Definition, Extern, GlobalKind, Term},
};
use derive_more::Display;
use index_vec::IndexVec;
use std::collections::{BTreeMap, HashMap, HashSet, hash_map::Entry};
use tja_macros::define_index_type;

pub(crate) struct Program {
    pub(crate) types: IndexVec<TypeIdx, Type>,
    pub(crate) globals: IndexVec<GlobalIdx, Global>,
    pub(crate) term_decls: IndexVec<TermDeclIdx, TermDecl>,
    pub(crate) rules: BTreeMap<TermDeclIdx, Vec<Rule>>,
}

define_index_type! {
    pub(crate) struct TypeIdx = usize;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Type {
    Builtin(BuiltinType),
    External(String),
    Tuple(Vec<TypeIdx>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display)]
pub(crate) enum BuiltinType {
    #[display("bool")]
    Bool,
    Int(IntType),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub(crate) enum IntType {
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
    ty: TypeIdx,
}

impl<'a> DisplayType<'a> {
    fn new(ctx: &'a LoweringCtx, ty: TypeIdx) -> Self {
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

define_index_type! {
    pub(crate) struct GlobalIdx = usize;
}

pub(crate) struct Global {
    pub(crate) name: String,
    pub(crate) ty: TypeIdx,
    pub(crate) kind: GlobalKind,
}

pub(crate) struct TermDecl {
    pub(crate) kind: TermDeclKind,
    pub(crate) name: String,
    pub(crate) arg_tys: Vec<TypeIdx>,
    pub(crate) ret_ty: TypeIdx,
    pub(crate) is_external: bool,
    pub(crate) is_pure: bool,
}

define_index_type! {
    struct OverloadTermIdx = usize;
}

struct OverloadTerm(Vec<TermDeclIdx>);

#[derive(Clone, Copy)]
enum TermSymbol {
    Concrete(TermDeclIdx),
    Overload(OverloadTermIdx),
}

pub(crate) struct Rule {
    pub(crate) pats: Vec<Pattern>,
    pub(crate) guards: Vec<Guard>,
    pub(crate) body: Body,
}

#[derive(Clone)]
enum ExpectedType {
    OneOf(HashSet<TypeIdx>),
    Any,
}

#[derive(Clone, Copy, PartialEq)]
enum ExprCtx {
    Guard,
    Body,
}

define_index_type! {
    pub(crate) struct VariableIdx = usize;
}

struct RuleLoweringCtx<'a> {
    ctx: &'a mut LoweringCtx,
    decl: TermDeclIdx,
    vars: IndexVec<VariableIdx, TypeIdx>,
    var_symbols: HashMap<&'a str, VariableIdx>,
}

impl<'a> RuleLoweringCtx<'a> {
    fn new(ctx: &'a mut LoweringCtx, decl: TermDeclIdx) -> Self {
        Self {
            ctx,
            decl,
            vars: IndexVec::new(),
            var_symbols: HashMap::new(),
        }
    }

    fn display_type<'s>(&'s self, ty: TypeIdx) -> DisplayType<'s> {
        DisplayType::new(self.ctx, ty)
    }

    fn lower_pat(&mut self, pat: &'a ast::Pattern, ty: TypeIdx) -> Pattern {
        match pat {
            ast::Pattern::Application { name, args } => {
                let Some(&symbol) = self.ctx.term_symbols.get(name) else {
                    panic!("term '{}' not defined", name);
                };
                let decl = match symbol {
                    TermSymbol::Concrete(decl) => {
                        let arg_tys = &self.ctx.term_decls[decl].arg_tys;

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
                    TermSymbol::Overload(term) => *self.ctx.overload_terms[term]
                        .0
                        .iter()
                        .find(|&&decl| self.ctx.term_decls[decl].arg_tys == &[ty])
                        .expect("no matching overload found"),
                };

                assert!(
                    self.ctx.term_decls[decl].is_pure,
                    "term '{}' can't be used in pattern; patterns must use pure terms",
                    self.ctx.term_decls[decl].name
                );

                let ty = self.ctx.term_decls[decl].ret_ty;
                let tys = match &self.ctx.types[ty] {
                    Type::Tuple(tys) => tys.clone(),
                    _ => vec![ty],
                };

                assert_eq!(
                    tys.len(),
                    args.len(),
                    "term arity mismatch: expected {}, got {}",
                    tys.len(),
                    args.len(),
                );

                Pattern::Application {
                    term_decl: decl,
                    pats: args
                        .iter()
                        .zip(tys)
                        .map(|(pat, ty)| self.lower_pat(pat, ty))
                        .collect(),
                }
            }
            ast::Pattern::Tuple(pats) => {
                let Type::Tuple(tys) = &self.ctx.types[ty] else {
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

                Pattern::Tuple(
                    pats.iter()
                        .zip(tys.clone())
                        .map(|(pat, ty)| self.lower_pat(pat, ty))
                        .collect(),
                )
            }
            &ast::Pattern::Literal(ast::Literal::Int { value, ty: int_ty }) => {
                let Type::Builtin(BuiltinType::Int(_)) = self.ctx.types[ty] else {
                    if let Some(int_ty) = int_ty {
                        let pat_ty = self
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
                    self.ctx.types[ty],
                    Type::Builtin(BuiltinType::Bool),
                    "pattern type mismatch: expected '{}', found 'bool'",
                    self.display_type(ty),
                );

                Pattern::Literal(Literal::Bool(value))
            }
            ast::Pattern::Ident(ident) => {
                let (pat, pat_ty) = match self.ctx.global_symbols.get(ident) {
                    Some(&global) => (Pattern::Global(global), Some(self.ctx.globals[global].ty)),
                    None => match self.var_symbols.entry(ident) {
                        Entry::Occupied(entry) => (
                            Pattern::Variable(*entry.get()),
                            Some(self.vars[*entry.get()]),
                        ),
                        Entry::Vacant(entry) => {
                            let var = self.vars.push(ty);

                            entry.insert(var);

                            (Pattern::Bind(var), None)
                        }
                    },
                };

                if let Some(pat_ty) = pat_ty {
                    assert_eq!(
                        ty,
                        pat_ty,
                        "pattern type mismatch: expected '{}', found '{}'",
                        self.display_type(ty),
                        self.display_type(pat_ty)
                    );
                }

                pat
            }
            ast::Pattern::Wildcard => Pattern::Wildcard,
        }
    }

    fn lower_expr_impl(
        &mut self,
        expr: &ast::Expr,
        expected_ty: ExpectedType,
        ctx: ExprCtx,
    ) -> Expr {
        let expr = match expr {
            ast::Expr::Call { name, args } => {
                let Some(&symbol) = self.ctx.term_symbols.get(name) else {
                    panic!("term '{}' not defined", name);
                };
                let (decl, args) = match symbol {
                    TermSymbol::Concrete(decl_idx) => {
                        let decl = &self.ctx.term_decls[decl_idx];

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

                        (decl_idx, args)
                    }
                    TermSymbol::Overload(overload) => {
                        let decls = self.ctx.overload_terms[overload].0.clone();
                        let compute_expected_ty = |this: &mut Self, needle: &[TypeIdx]| {
                            ExpectedType::OneOf(
                                decls
                                    .iter()
                                    .filter_map(|&decl| {
                                        let tys = &this.ctx.term_decls[decl].arg_tys;

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

                            tys.push(expr.ty);
                            exprs.push(expr);
                        }

                        let decl = decls
                            .into_iter()
                            .find(|&decl| self.ctx.term_decls[decl].arg_tys == tys)
                            .unwrap();

                        (decl, exprs)
                    }
                };
                let decls = &self.ctx.term_decls;

                if ctx == ExprCtx::Body
                    && decls[self.decl].kind == TermDeclKind::Total
                    && matches!(
                        decls[decl].kind,
                        TermDeclKind::Partial | TermDeclKind::Multi
                    )
                {
                    panic!(
                        "term '{}' can't be used in total term rule '{}'; total term rules must use only total terms in body",
                        decls[decl].name, decls[self.decl].name
                    );
                }

                if ctx == ExprCtx::Guard && !decls[decl].is_pure {
                    panic!(
                        "term '{}' can't be used in guard expression; guards must use pure terms",
                        decls[decl].name
                    );
                }

                Expr {
                    kind: ExprKind::Call {
                        term_decl: decl,
                        args,
                    },
                    ty: decls[decl].ret_ty,
                }
            }
            ast::Expr::Tuple(exprs) => {
                let compute_expected_ty = |this: &mut Self, needle: &[TypeIdx]| match &expected_ty {
                    ExpectedType::OneOf(tys) => ExpectedType::OneOf(
                        tys.iter()
                            .filter_map(|&ty| match &this.ctx.types[ty] {
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

                    tys.push(expr.ty);
                    lowered_exprs.push(expr);
                }

                let ty = self.ctx.intern_type(Type::Tuple(tys));

                Expr {
                    kind: ExprKind::Tuple(lowered_exprs),
                    ty,
                }
            }
            &ast::Expr::TupleIndex { ref expr, idx } => {
                let expr = self.lower_expr_impl(expr, ExpectedType::Any, ctx);
                let ty = expr.ty;
                let ty = match &self.ctx.types[ty] {
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

                Expr {
                    kind: ExprKind::TupleIndex {
                        expr: Box::new(expr),
                        idx,
                    },
                    ty,
                }
            }
            &ast::Expr::Literal(ast::Literal::Int { value, ty }) => {
                let expected_ty = match expected_ty {
                    ExpectedType::OneOf(tys) => {
                        match tys
                            .iter()
                            .filter_map(|&ty| match self.ctx.types[ty] {
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
                    self.ctx
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

                return Expr {
                    kind: ExprKind::Literal(Literal::Int(value)),
                    ty,
                };
            }
            &ast::Expr::Literal(ast::Literal::Bool(value)) => Expr {
                kind: ExprKind::Literal(Literal::Bool(value)),
                ty: self.ctx.intern_type(Type::Builtin(BuiltinType::Bool)),
            },
            ast::Expr::Ident(ident) => match self.ctx.global_symbols.get(ident) {
                Some(&global) => Expr {
                    kind: ExprKind::Global(global),
                    ty: self.ctx.globals[global].ty,
                },
                None => self
                    .var_symbols
                    .get(ident.as_str())
                    .map(|&var| Expr {
                        kind: ExprKind::Variable(var),
                        ty: self.vars[var],
                    })
                    .unwrap_or_else(|| panic!("variable '{}' not found", ident)),
            },
        };

        if let ExpectedType::OneOf(tys) = expected_ty {
            let ty = expr.ty;
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

    fn lower_expr(&mut self, expr: &ast::Expr, expected_ty: Option<TypeIdx>, ctx: ExprCtx) -> Expr {
        self.lower_expr_impl(
            expr,
            expected_ty
                .map(|ty| ExpectedType::OneOf(HashSet::from([ty])))
                .unwrap_or(ExpectedType::Any),
            ctx,
        )
    }

    fn lower(mut self, pats: &'a [ast::Pattern], rule: &'a ast::Rule) -> Rule {
        let decl = &self.ctx.term_decls[self.decl];
        let ret_ty = decl.ret_ty;

        assert_eq!(
            decl.arg_tys.len(),
            pats.len(),
            "root rule pattern arity mismatch: expected {}, got {}",
            decl.arg_tys.len(),
            pats.len()
        );

        let mut lowered_pats = Vec::with_capacity(pats.len());

        for (pat, ty) in pats.iter().zip(decl.arg_tys.clone()) {
            lowered_pats.push(self.lower_pat(pat, ty));
        }

        let guards = rule
            .guards
            .iter()
            .map(|ast::Guard { pat, expr }| {
                let expr = self.lower_expr(expr, None, ExprCtx::Guard);
                let pat = self.lower_pat(pat, expr.ty);

                Guard { pat, expr }
            })
            .collect();
        let body = {
            let bindings = rule
                .body
                .lets
                .iter()
                .map(|ast::Let { name, ty, value }| {
                    let ty = self.ctx.lower_type(ty);
                    let expr = self.lower_expr(value, Some(ty), ExprCtx::Body);
                    let var = self.vars.push(expr.ty);

                    assert!(
                        self.var_symbols.insert(name, var).is_none(),
                        "redefinition of variable '{}'",
                        name
                    );

                    (var, expr)
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

#[derive(Clone)]
pub(crate) enum Pattern {
    Application {
        term_decl: TermDeclIdx,
        pats: Vec<Self>,
    },
    Tuple(Vec<Self>),
    Literal(Literal),
    Bind(VariableIdx),
    Variable(VariableIdx),
    Global(GlobalIdx),
    Wildcard,
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
        args: Vec<Expr>,
    },
    Global(GlobalIdx),
    Literal(Literal),
    Tuple(Vec<Expr>),
    TupleIndex {
        expr: Box<Expr>,
        idx: usize,
    },
    Variable(VariableIdx),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) enum Literal {
    Int(i64),
    Bool(bool),
}

pub(crate) struct Guard {
    pub(crate) pat: Pattern,
    pub(crate) expr: Expr,
}

pub(crate) struct Body {
    pub(crate) bindings: Vec<(VariableIdx, Expr)>,
    pub(crate) result: Expr,
}

struct LoweringCtx {
    types: IndexVec<TypeIdx, Type>,
    type_dedup: HashMap<Type, TypeIdx>,
    type_symbols: HashMap<String, TypeIdx>,

    globals: IndexVec<GlobalIdx, Global>,
    global_symbols: HashMap<String, GlobalIdx>,

    term_decls: IndexVec<TermDeclIdx, TermDecl>,
    overload_terms: IndexVec<OverloadTermIdx, OverloadTerm>,
    term_symbols: HashMap<String, TermSymbol>,

    rules: BTreeMap<TermDeclIdx, Vec<Rule>>,
}

impl LoweringCtx {
    fn new() -> Self {
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
        let mut types = IndexVec::with_capacity(predefined.len());
        let mut type_dedup = HashMap::with_capacity(predefined.len());
        let mut type_symbols = HashMap::with_capacity(predefined.len());

        for (name, ty) in predefined {
            let ty = Type::Builtin(ty);
            let idx = types.push(ty.clone());

            type_dedup.insert(ty, idx);
            type_symbols.insert(name.into(), idx);
        }

        Self {
            types,
            type_dedup,
            type_symbols,

            globals: IndexVec::new(),
            global_symbols: HashMap::new(),

            term_decls: IndexVec::new(),
            overload_terms: IndexVec::new(),
            term_symbols: HashMap::new(),

            rules: BTreeMap::new(),
        }
    }

    fn lower_types(&mut self, defs: &[Definition]) {
        for def in defs {
            match def {
                Definition::Extern(Extern::Type {
                    name,
                    external_name,
                }) => {
                    let idx = self.types.push(Type::External(
                        external_name.as_ref().unwrap_or(name).clone(),
                    ));

                    assert!(
                        self.type_symbols.insert(name.clone(), idx).is_none(),
                        "redefinition of type '{name}'",
                    );
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
                    let ty = self.lower_type(ty);
                    let idx = self.globals.push(Global {
                        name: external_name.clone(),
                        ty,
                        kind: *kind,
                    });

                    assert!(
                        self.global_symbols.insert(name.clone(), idx).is_none(),
                        "redefinition of global '{name}'",
                    );
                }
                _ => (),
            }
        }
    }

    fn intern_type(&mut self, ty: Type) -> TypeIdx {
        *self
            .type_dedup
            .entry(ty)
            .or_insert_with_key(|ty| self.types.push(ty.clone()))
    }

    fn lower_type(&mut self, ty: &ast::Type) -> TypeIdx {
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
            let idx = self.term_decls.push(decl);

            assert!(
                self.term_symbols
                    .insert(term.name.clone(), TermSymbol::Concrete(idx))
                    .is_none(),
                "redefinition of term '{}'",
                term.name
            );
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

                    let idx = self.overload_terms.push(OverloadTerm(decls));

                    assert!(
                        self.term_symbols
                            .insert(term.name.clone(), TermSymbol::Overload(idx))
                            .is_none(),
                        "redefinition of overload term '{}'",
                        term.name,
                    );
                }
                _ => (),
            }
        }
    }

    fn lower_rules(&mut self, defs: &[Definition]) {
        for def in defs {
            match def {
                Definition::Rule(rule) => {
                    let (term, pats) = match &rule.pat {
                        ast::Pattern::Application { name, args } => (name, args),
                        _ => panic!("rule's root pattern must be application"),
                    };
                    let decl = match self.term_symbols.get(term) {
                        Some(&TermSymbol::Concrete(decl)) => {
                            assert!(
                                !self.term_decls[decl].is_external,
                                "rules aren't allowed for external terms"
                            );

                            decl
                        }
                        Some(TermSymbol::Overload(_)) => {
                            panic!("rule aren't allowed for overload terms");
                        }
                        None => panic!("term '{}' not defined", term),
                    };
                    let rule = RuleLoweringCtx::new(self, decl).lower(pats, rule);

                    self.rules.entry(decl).or_default().push(rule);
                }
                _ => (),
            }
        }
    }

    fn lower(mut self, defs: Vec<Definition>) -> Program {
        self.lower_types(&defs);
        self.lower_globals(&defs);
        self.lower_concrete_terms(&defs);
        self.lower_overload_terms(&defs);
        self.lower_rules(&defs);

        for (idx, decl) in self.term_decls.iter_enumerated() {
            if !decl.is_external {
                assert!(
                    self.rules.contains_key(&idx),
                    "internal term '{}' must have at least 1 rule",
                    decl.name
                );
            }
        }

        Program {
            types: self.types,
            globals: self.globals,
            term_decls: self.term_decls,
            rules: self.rules,
        }
    }
}

pub(crate) fn run(defs: Vec<Definition>) -> Program {
    LoweringCtx::new().lower(defs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{grammar::DefinitionsParser, lexer::Lexer};

    // NOTE: ideally diagnostic messages should be emitted in such cases instead
    // of panics, but it works for now
    macro_rules! expect_panic {
        ($name: ident, $input: expr, $expected: expr) => {
            #[test]
            #[should_panic(expected = $expected)]
            fn $name() {
                let defs = DefinitionsParser::new()
                    .parse(Lexer::new($input))
                    .expect("must parse");

                run(defs);
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
        extern_term_rule,
        r"
            extern total term foo(usize) -> bool;

            rule foo(0) {
                false
            }
        ",
        "rules aren't allowed for external terms"
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
            pure partial term bar(usize) -> (usize, i8);

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
        impure_term_application,
        r"
            total term foo(usize) -> ();
            partial term bar(usize) -> ();

            rule foo(bar()) {
                ()
            }
        ",
        "term 'bar' can't be used in pattern; patterns must use pure terms"
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
        udefined_variable,
        r"
            total term foo(usize) -> ();

            rule foo(69) {
                bar
            }
        ",
        "variable 'bar' not found"
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

    expect_panic!(
        internal_term_without_rules,
        r"
            total term foo(bool) -> u8;
        ",
        "internal term 'foo' must have at least 1 rule"
    );
}
