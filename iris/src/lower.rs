use crate::{
    ast::{self, Definition, Expr, Literal, Pattern},
    visitor::{Visitor, walk_pat},
};
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
};

#[derive(Debug, Default)]
pub struct Module {
    pub rules: BTreeMap<String, Vec<Rule>>,
    pub consts: HashMap<String, Type>,
    pub types: HashMap<String, Type>,
    pub decls: BTreeMap<String, Declaration>,
}

impl Module {
    fn add_type(&mut self, name: String, external_name: String) {
        assert!(
            self.types
                .insert(name.clone(), Type::External(external_name))
                .is_none(),
            "redefinition of type '{name}'"
        );
    }

    fn add_const(&mut self, name: String, ty: Type) {
        assert!(
            self.consts.insert(name.clone(), ty).is_none(),
            "redefinition of const '{name}'",
        );
    }

    fn add_decl(&mut self, name: String, arg_tys: Vec<Type>, ret_ty: Type, partial: bool) {
        assert!(
            self.decls
                .insert(
                    name.clone(),
                    Declaration {
                        arg_tys,
                        ret_ty,
                        constructor: None,
                        extractor: None,
                        partial
                    }
                )
                .is_none(),
            "redefinition of decl '{name}'"
        );
    }
}

#[derive(Debug)]
pub struct Declaration {
    pub arg_tys: Vec<Type>,
    pub ret_ty: Type,
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
pub struct Rule {
    pub args: Vec<Pattern>,
    pub expr: Expr,
    pub priority: i64,
}

fn get_ty(module: &Module, name: &str) -> Type {
    match name {
        "bool" => Type::Builtin(BuiltinTy::Bool),
        "u8" => Type::Builtin(BuiltinTy::U8),
        "u16" => Type::Builtin(BuiltinTy::U16),
        "u32" => Type::Builtin(BuiltinTy::U32),
        "u64" => Type::Builtin(BuiltinTy::U64),
        "usize" => Type::Builtin(BuiltinTy::Usize),
        "i8" => Type::Builtin(BuiltinTy::I8),
        "i16" => Type::Builtin(BuiltinTy::I16),
        "i32" => Type::Builtin(BuiltinTy::I32),
        "i64" => Type::Builtin(BuiltinTy::I64),
        "isize" => Type::Builtin(BuiltinTy::Isize),
        _ => module
            .types
            .get(name)
            .expect(&format!("type {name} is not defined"))
            .clone(),
    }
}

pub fn run(definitions: Vec<Definition>) -> Module {
    let mut module = Module::default();

    for def in definitions {
        match def {
            Definition::Declaration(ast::Declaration {
                name,
                arg_tys,
                ret_ty,
                partial,
            }) => {
                let arg_tys = arg_tys.iter().map(|name| get_ty(&module, name)).collect();
                let ret_ty = get_ty(&module, &ret_ty);

                module.add_decl(name.clone(), arg_tys, ret_ty, partial);
            }
            Definition::Extern(_extern) => match _extern {
                ast::Extern::Constructor {
                    name,
                    external_name,
                } => {
                    let decl = module.decls.get_mut(&name).unwrap();

                    decl.constructor = Some(Constructor {
                        name: external_name,
                    });
                }
                ast::Extern::Extractor {
                    name,
                    external_name,
                    infallible,
                } => {
                    let decl = module.decls.get_mut(&name).unwrap();

                    decl.extractor = Some(Extractor {
                        name: external_name,
                        infallible,
                    });
                }
                ast::Extern::Const { name, ty } => {
                    module.add_const(name, get_ty(&module, &ty));
                }
            },
            Definition::Type(ast::Type {
                name,
                external_name,
            }) => module.add_type(name, external_name),
            Definition::Rule(rule) => {
                let check_pat = CheckPattern { module: &module };
                check_pat.visit_pat(&rule.pat);

                if let Pattern::Application { name, args } = rule.pat {
                    module.rules.entry(name).or_default().push(Rule {
                        args,
                        expr: rule.expr,
                        priority: rule.priority.unwrap_or(0),
                    });
                } else {
                    panic!("lhs root pattern must be application");
                }
            }
        }
    }

    module
}

struct CheckPattern<'a> {
    module: &'a Module,
}

impl Visitor for CheckPattern<'_> {
    fn visit_pat(&self, pat: &Pattern) {
        match pat {
            Pattern::Application { name, args } => {
                assert!(
                    self.module.decls.get(name).is_some(),
                    "undefined extractor '{name}'"
                );

                let arg_tys = &self.module.decls[name].arg_tys;

                assert_eq!(arg_tys.len(), args.len(), "arity mismatch");

                for (pat, ty) in args.iter().zip(arg_tys) {
                    if let Some(pat_ty) = self.pat_ty(pat) {
                        assert_eq!(&pat_ty, ty, "type mismatch");
                    }
                }
            }
            Pattern::Literal(Literal::Const(name)) => {
                assert!(
                    self.module.consts.contains_key(name),
                    "undefined const '{name}'"
                );
            }
            Pattern::Literal(_) | Pattern::Ident(_) | Pattern::Wildcard => (),
        }

        walk_pat(self, pat);
    }
}

impl CheckPattern<'_> {
    // TODO: change `Option<Type>` to `Type` and handle `Literal::Int`
    fn pat_ty(&self, pat: &Pattern) -> Option<Type> {
        match pat {
            Pattern::Application { name, .. } => Some(self.module.decls[name].ret_ty.clone()),
            Pattern::Literal(lit) => match lit {
                Literal::Int(_) => None,
                Literal::Bool(_) => Some(Type::Builtin(BuiltinTy::Bool)),
                Literal::Const(name) => Some(self.module.consts[name].clone()),
            },
            Pattern::Ident(_) | Pattern::Wildcard => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Builtin(BuiltinTy),
    External(String),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin(ty) => std::fmt::Display::fmt(ty, f),
            Self::External(name) => write!(f, "{name}"),
        }
    }
}
