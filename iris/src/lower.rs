use crate::{
    ast::{self, Body, Definition, Guard, Let, Literal, Pattern},
    decision::{self, Decision},
};
use index_vec::{IndexVec, define_index_type, index_vec};
use std::collections::{BTreeMap, HashMap};

define_index_type! {
    pub struct ConstIdx = usize;
}

define_index_type! {
    pub struct TyIdx = usize;
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum BuiltinTy {
    Bool,
    U8,
    U16,
    U32,
    U64,
    Usize,
    I8,
    I16,
    I32,
    I64,
    Isize,
}

impl std::fmt::Display for BuiltinTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::Usize => write!(f, "usize"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Isize => write!(f, "isize"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Builtin(BuiltinTy),
    External(String),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin(ty) => ty.fmt(f),
            Self::External(name) => name.fmt(f),
        }
    }
}

define_index_type! {
    pub struct ExprIdx = usize;
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
    Integer { value: i64, ty: TyIdx },
    Parameter(usize),
    Bool(bool),
    Const(ConstIdx),
    Constructor { decl: DeclIdx, args: Vec<ExprIdx> },
    Extractor { decl: DeclIdx, arg: ExprIdx },
    MatchSome(ExprIdx),
    MakeSome(ExprIdx),
    TupleIndex { expr: ExprIdx, idx: usize },
}

#[derive(Debug)]
pub struct RuleSet {
    pub decl: DeclIdx,
    pub tree: Decision,
    pub exprs: IndexVec<ExprIdx, Expr>,
}

define_index_type! {
    pub struct DeclIdx = usize;
}

#[derive(Debug)]
pub struct Declaration {
    pub name: String,
    pub arg_tys: Vec<TyIdx>,
    pub ret_ty: TyIdx,
    pub partial: bool,
    pub constructor: Option<Constructor>,
    pub extractor: Option<Extractor>,
}

#[derive(Debug)]
pub struct Constructor {
    pub name: String,
}

#[derive(Debug)]
pub struct Extractor {
    pub name: String,
    pub infallible: bool,
}

#[derive(Debug)]
pub struct Const {
    pub name: String,
    pub ty: TyIdx,
}

#[derive(Debug, Default)]
pub struct Module {
    pub rulesets: Vec<RuleSet>,
    pub consts: IndexVec<ConstIdx, Const>,
    pub types: IndexVec<TyIdx, Type>,
    pub decls: IndexVec<DeclIdx, Declaration>,
}

impl From<ModuleLowerer> for Module {
    fn from(lowerer: ModuleLowerer) -> Self {
        Self {
            rulesets: lowerer.rulesets,
            consts: lowerer.consts,
            types: lowerer.types,
            decls: lowerer.decls,
        }
    }
}

define_index_type! {
    pub struct RuleIdx = usize;
}

#[derive(Debug)]
pub struct Rule<'a> {
    pub args: Vec<Pattern>,
    pub priority: i64,
    pub guards: &'a [Guard],
    pub body: &'a Body,
}

#[derive(Default)]
pub struct ModuleLowerer {
    pub types: IndexVec<TyIdx, Type>,
    pub type_map: HashMap<String, TyIdx>,
    pub consts: IndexVec<ConstIdx, Const>,
    pub const_map: HashMap<String, ConstIdx>,
    pub decls: IndexVec<DeclIdx, Declaration>,
    pub decl_map: BTreeMap<String, DeclIdx>,
    pub rulesets: Vec<RuleSet>,
}

impl ModuleLowerer {
    fn new() -> Self {
        let predefined_tys = [
            ("bool", BuiltinTy::Bool),
            ("u8", BuiltinTy::U8),
            ("u16", BuiltinTy::U16),
            ("u32", BuiltinTy::U32),
            ("u64", BuiltinTy::U64),
            ("usize", BuiltinTy::Usize),
            ("i8", BuiltinTy::I8),
            ("i16", BuiltinTy::I16),
            ("i32", BuiltinTy::I32),
            ("i64", BuiltinTy::I64),
            ("isize", BuiltinTy::Isize),
        ];

        let mut types = IndexVec::new();
        let mut type_map = HashMap::new();

        for (name, ty) in predefined_tys {
            let idx = types.push(Type::Builtin(ty));

            type_map.insert(name.to_string(), idx);
        }

        Self {
            types,
            type_map,
            ..Default::default()
        }
    }

    fn add_type(&mut self, name: String, external_name: String) {
        let idx = self.types.push(Type::External(external_name));

        assert!(
            self.type_map.insert(name.clone(), idx).is_none(),
            "redefinition of type '{name}'"
        );
    }

    fn add_const(&mut self, name: String, ty: TyIdx) {
        let idx = self.consts.push(Const {
            name: name.clone(),
            ty,
        });

        assert!(
            self.const_map.insert(name.clone(), idx).is_none(),
            "redefinition of const '{name}'",
        );
    }

    fn add_decl(&mut self, name: String, arg_tys: Vec<TyIdx>, ret_ty: TyIdx, partial: bool) {
        let idx = self.decls.push(Declaration {
            name: name.clone(),
            arg_tys,
            ret_ty,
            constructor: None,
            extractor: None,
            partial,
        });

        assert!(
            self.decl_map.insert(name.clone(), idx).is_none(),
            "redefinition of decl '{name}'"
        );
    }

    fn expect_decl(&self, name: &str) -> &Declaration {
        &self.decls[self.decl_map[name]]
    }

    fn expect_decl_mut(&mut self, name: &str) -> &mut Declaration {
        &mut self.decls[self.decl_map[name]]
    }

    fn lower_ty(&self, name: &str) -> TyIdx {
        *self
            .type_map
            .get(name)
            .expect(&format!("type '{name}' is not defined"))
    }

    fn lower_types(&mut self, definitions: &[Definition]) {
        for def in definitions {
            match def {
                Definition::Type(ast::Type {
                    name,
                    external_name,
                }) => self.add_type(name.clone(), external_name.clone()),
                _ => (),
            }
        }
    }

    fn lower_declarations(&mut self, definitions: &[Definition]) {
        for def in definitions {
            match def {
                Definition::Declaration(ast::Declaration {
                    name,
                    arg_tys,
                    ret_ty,
                    partial,
                }) => {
                    let arg_tys = arg_tys.iter().map(|name| self.lower_ty(name)).collect();
                    let ret_ty = self.lower_ty(ret_ty);

                    self.add_decl(name.clone(), arg_tys, ret_ty, *partial);
                }
                _ => (),
            }
        }
    }

    fn lower_externs(&mut self, definitions: &[Definition]) {
        for def in definitions {
            match def {
                Definition::Extern(extern_) => match extern_ {
                    ast::Extern::Constructor {
                        name,
                        external_name,
                    } => {
                        let decl = self.expect_decl_mut(name);

                        decl.constructor = Some(Constructor {
                            name: external_name.clone(),
                        });
                    }
                    ast::Extern::Extractor {
                        name,
                        external_name,
                        infallible,
                    } => {
                        let decl = self.expect_decl_mut(name);

                        decl.extractor = Some(Extractor {
                            name: external_name.clone(),
                            infallible: *infallible,
                        });
                    }
                    ast::Extern::Const { name, ty } => {
                        self.add_const(name.clone(), self.lower_ty(ty));
                    }
                },
                _ => (),
            }
        }
    }

    fn lower_rules(&mut self, definitions: &[Definition]) {
        let mut rulesets: BTreeMap<DeclIdx, IndexVec<RuleIdx, Rule>> = BTreeMap::new();

        for def in definitions {
            match def {
                Definition::Rule(rule) => {
                    // TODO: add type checking pls :)
                    if let Pattern::Application { name, args } = &rule.pat {
                        let decl = self.decl_map[name];

                        rulesets.entry(decl).or_default().push(Rule {
                            args: args.clone(),
                            body: &rule.body,
                            guards: &rule.guards,
                            priority: rule.priority.unwrap_or(0),
                        });
                    } else {
                        panic!("lhs root pattern must be application");
                    }
                }
                _ => (),
            }
        }

        for rules in rulesets.values_mut() {
            rules.sort_unstable_by_key(|rule| std::cmp::Reverse(rule.priority));
        }

        self.rulesets = rulesets
            .into_iter()
            .map(|(decl, rules)| RuleSetLowerer::lower(self, decl, rules))
            .collect();
    }

    // TODO: find a better place for this function or think about a different
    // way of getting the bool type value?
    fn bool_ty(&self) -> TyIdx {
        let idx = TyIdx::new(0);

        assert!(self.types[idx] == Type::Builtin(BuiltinTy::Bool));

        idx
    }
}

pub type EnvIdx = RuleIdx;

pub struct RuleSetLowerer<'a> {
    pub lowerer: &'a ModuleLowerer,
    pub decl: DeclIdx,
    pub exprs: IndexVec<ExprIdx, Expr>,
    pub expr_map: HashMap<Expr, ExprIdx>,
    pub envs: HashMap<EnvIdx, HashMap<String, ExprIdx>>,
}

impl<'a> RuleSetLowerer<'a> {
    fn lower(
        lowerer: &'a ModuleLowerer,
        decl: DeclIdx,
        rules: IndexVec<RuleIdx, Rule<'_>>,
    ) -> RuleSet {
        let mut lowerer = Self {
            lowerer,
            decl,
            exprs: IndexVec::new(),
            expr_map: HashMap::new(),
            envs: index_vec![HashMap::new(); rules.len()]
                .into_iter_enumerated()
                .collect(),
        };
        let tree = decision::compile(&mut lowerer, rules);

        RuleSet {
            decl: lowerer.decl,
            tree,
            exprs: lowerer.exprs,
        }
    }

    pub fn create_expr(&mut self, expr: Expr) -> ExprIdx {
        *self
            .expr_map
            .entry(expr.clone())
            .or_insert(self.exprs.push(expr))
    }

    pub fn bind_var(&mut self, env: EnvIdx, name: String, expr: ExprIdx) {
        assert!(
            self.envs
                .get_mut(&env)
                .unwrap()
                .insert(name, expr)
                .is_none()
        );
    }

    pub fn lower_body(&mut self, env: EnvIdx, body: &Body, ty: TyIdx) -> (Vec<ExprIdx>, ExprIdx) {
        let mut bindings = Vec::new();

        for Let { name, ty, value } in &body.lets {
            let expr = self.lower_expr(&value, env, Some(self.lowerer.lower_ty(ty)));

            self.bind_var(env, name.clone(), expr);
            bindings.push(expr);
        }

        let mut expr = self.lower_expr(&body.expr, env, Some(ty));

        if self.lowerer.decls[self.decl].partial {
            expr = self.create_expr(Expr::MakeSome(expr));
        }

        (bindings, expr)
    }

    pub fn lower_expr(&mut self, expr: &ast::Expr, env: EnvIdx, ty: Option<TyIdx>) -> ExprIdx {
        match expr {
            ast::Expr::Call { name, args } => {
                let decl = self.lowerer.expect_decl(name);
                let tys = &decl.arg_tys;
                let args: Vec<_> = args
                    .iter()
                    .zip(tys)
                    .map(|(expr, ty)| self.lower_expr(expr, env, Some(ty.clone())))
                    .collect();
                let expr = self.create_expr(Expr::Constructor {
                    decl: self.lowerer.decl_map[name],
                    args,
                });

                if decl.partial {
                    self.create_expr(Expr::MatchSome(expr))
                } else {
                    expr
                }
            }
            ast::Expr::Literal(literal) => match literal.clone() {
                Literal::Int(value) => self.create_expr(Expr::Integer {
                    value,
                    ty: ty.unwrap(),
                }),
                Literal::Bool(value) => self.create_expr(Expr::Bool(value)),
                Literal::Const(name) => {
                    self.create_expr(Expr::Const(self.lowerer.const_map[&name]))
                }
            },
            ast::Expr::Ident(ident) => self.envs[&env][ident],
        }
    }

    pub fn expr_ty(&self, expr: ExprIdx) -> Option<TyIdx> {
        match &self.exprs[expr] {
            Expr::Integer { ty, .. } => Some(*ty),
            Expr::Parameter(idx) => Some(self.lowerer.decls[self.decl].arg_tys[*idx]),
            Expr::Bool(_) => Some(self.lowerer.bool_ty()),
            Expr::Const(const_) => Some(self.lowerer.consts[*const_].ty),
            Expr::Constructor { decl, .. } => Some(self.lowerer.decls[*decl].ret_ty),
            Expr::Extractor { .. } => None,
            Expr::MakeSome(expr) | Expr::MatchSome(expr) => self.expr_ty(*expr),
            Expr::TupleIndex { expr, idx } => {
                fn extractor_decl(lowerer: &RuleSetLowerer, expr: ExprIdx) -> DeclIdx {
                    match &lowerer.exprs[expr] {
                        Expr::MatchSome(expr) => extractor_decl(lowerer, *expr),
                        Expr::Extractor { decl, .. } => *decl,
                        _ => unreachable!(),
                    }
                }

                Some(self.lowerer.decls[extractor_decl(self, *expr)].arg_tys[*idx])
            }
        }
    }
}

pub fn run(definitions: Vec<Definition>) -> Module {
    let mut lowerer = ModuleLowerer::new();

    lowerer.lower_types(&definitions);
    lowerer.lower_declarations(&definitions);
    lowerer.lower_externs(&definitions);
    lowerer.lower_rules(&definitions);

    lowerer.into()
}
