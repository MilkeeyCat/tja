use crate::{
    decision::{Condition, Decision},
    lower::{Expr, ExprIdx, Module, RuleSet, TyIdx},
};
use indexmap::{IndexSet, indexset};
use std::fmt::{Arguments, Write};

macro_rules! write_indended {
    ($dst:expr, $($arg:tt)*) => {
        $dst.write_indended(format_args!($($arg)*))
    };
}

macro_rules! writeln_indended {
    ($dst:expr $(,)?) => {
        write_indended!($dst, "\n")
    };
    ($dst:expr, $($arg:tt)*) => {
        $dst.write_indended_nl(format_args!($($arg)*))
    };
}

struct IndentBuffer<W: Write> {
    buf: W,
    indent: usize,
}

impl<'a, W: Write> IndentBuffer<W> {
    fn new(buf: W) -> Self {
        Self { buf, indent: 0 }
    }
}

impl<W: Write> IndentBuffer<W> {
    fn write_indended(&mut self, args: Arguments<'_>) -> std::fmt::Result {
        write!(self.buf, "{}", " ".repeat(self.indent))?;

        self.buf.write_fmt(args)
    }

    fn write_indended_nl(&mut self, args: Arguments<'_>) -> std::fmt::Result {
        write!(self.buf, "{}", " ".repeat(self.indent))?;
        self.buf.write_fmt(args)?;

        write!(self.buf, "\n")
    }
}

impl<W: Write> Write for IndentBuffer<W> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.buf.write_str(s)
    }
}

trait Generator<W: Write> {
    fn buf_mut(&mut self) -> &mut IndentBuffer<W>;

    fn with_indent<F: FnOnce(&mut Self) -> std::fmt::Result>(&mut self, f: F) -> std::fmt::Result {
        self.buf_mut().indent += 4;
        f(self)?;
        self.buf_mut().indent -= 4;

        Ok(())
    }
}

struct ModuleGenerator<'a, W: Write> {
    module: &'a Module,
    buf: &'a mut IndentBuffer<W>,
}

impl<'a, W: Write> Generator<W> for ModuleGenerator<'a, W> {
    fn buf_mut(&mut self) -> &mut IndentBuffer<W> {
        self.buf
    }
}

impl<'a, W: Write> ModuleGenerator<'a, W> {
    fn new(module: &'a Module, buf: &'a mut IndentBuffer<W>) -> Self {
        Self { module, buf }
    }

    fn generate_context_trait(&mut self) -> std::fmt::Result {
        writeln!(self.buf, "pub trait Context {{")?;

        self.with_indent(|generator| {
            for decl in &self.module.decls {
                if let Some(ctor) = &decl.constructor {
                    let params = decl
                        .arg_tys
                        .iter()
                        .enumerate()
                        .map(|(idx, ty)| format!("p{idx}: {}", generator.ty_name(*ty)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let ret_ty = if decl.partial {
                        format!("Option<{}>", generator.ty_name(decl.ret_ty))
                    } else {
                        generator.ty_name(decl.ret_ty)
                    };

                    writeln_indended!(
                        generator.buf,
                        "fn {}(&mut self, {}) -> {};",
                        ctor.name,
                        params,
                        ret_ty,
                    )?;
                }

                if let Some(etor) = &decl.extractor {
                    let ret_tys: Vec<_> = decl
                        .arg_tys
                        .iter()
                        .map(|ty| generator.ty_name(*ty))
                        .collect();
                    let mut ret_tys = if ret_tys.len() > 1 {
                        format!("({})", ret_tys.join(", "))
                    } else {
                        ret_tys.join(", ")
                    };

                    if !etor.infallible {
                        ret_tys = format!("Option<{}>", ret_tys);
                    }

                    writeln_indended!(
                        generator.buf,
                        "fn {}(&mut self, p: {}) -> {};",
                        etor.name,
                        generator.ty_name(decl.ret_ty),
                        ret_tys,
                    )?;
                }
            }

            Ok(())
        })?;

        writeln!(self.buf, "}}")?;
        writeln!(self.buf)
    }

    fn generate_ruleset(&mut self, ruleset: &'a RuleSet) -> std::fmt::Result {
        let decl = &self.module.decls[ruleset.decl];
        let params = decl
            .arg_tys
            .iter()
            .enumerate()
            .map(|(idx, &ty)| format!("p{}: {}", idx, self.ty_name(ty)))
            .collect::<Vec<_>>()
            .join(", ");
        let ret_ty = if decl.partial {
            format!("Option<{}>", self.ty_name(decl.ret_ty))
        } else {
            self.ty_name(decl.ret_ty)
        };

        writeln!(
            self.buf,
            "fn {}_ctor<C: Context>(ctx: &mut C, {params}) -> {} {{",
            decl.name, ret_ty
        )?;

        self.with_indent(|generator| {
            let mut generator = RuleSetGenerator::new(generator, ruleset);

            generator.generate_decision(&ruleset.tree)?;

            if !all_paths_return_value(&ruleset.tree) {
                writeln!(generator.buf)?;
                writeln_indended!(generator.buf, "unreachable!();")?;
            }

            Ok(())
        })?;

        writeln!(self.buf, "}}")
    }

    fn ty_name(&self, ty: TyIdx) -> String {
        self.module.types[ty].to_string()
    }
}

struct RuleSetGenerator<'a, W: Write> {
    module: &'a Module,
    ruleset: &'a RuleSet,
    buf: &'a mut IndentBuffer<W>,
    bound_exprs: IndexSet<ExprIdx>,
}

impl<W: Write> Generator<W> for RuleSetGenerator<'_, W> {
    fn buf_mut(&mut self) -> &mut IndentBuffer<W> {
        self.buf
    }
}

impl<'a, W: Write> RuleSetGenerator<'a, W> {
    fn new(generator: &'a mut ModuleGenerator<W>, ruleset: &'a RuleSet) -> Self {
        Self {
            module: generator.module,
            ruleset,
            buf: generator.buf,
            bound_exprs: IndexSet::new(),
        }
    }

    fn generate_lets<F: FnOnce(&mut Self) -> std::fmt::Result>(
        &mut self,
        exprs: &IndexSet<ExprIdx>,
        f: F,
    ) -> std::fmt::Result {
        let bound_exprs = self.bound_exprs.clone();
        let mut exprs = exprs.difference(&bound_exprs).cloned();

        match exprs.next() {
            Some(expr) => {
                write_indended!(self.buf, "let v{} = ", expr.index())?;
                self.generate_expr(expr)?;
                writeln!(self.buf, ";")?;

                self.with_bound_exprs(&indexset![expr], |generator| {
                    generator.generate_lets(&exprs.collect(), f)
                })
            }
            None => f(self),
        }
    }

    fn with_bound_exprs<F: FnOnce(&mut Self) -> std::fmt::Result>(
        &mut self,
        exprs: &IndexSet<ExprIdx>,
        f: F,
    ) -> std::fmt::Result {
        let exprs: IndexSet<ExprIdx> = exprs.difference(&self.bound_exprs).cloned().collect();

        self.bound_exprs.extend(exprs.clone());
        f(self)?;
        self.bound_exprs = self.bound_exprs.difference(&exprs).cloned().collect();

        Ok(())
    }

    fn decompose_expr(&self, expr: ExprIdx) -> IndexSet<ExprIdx> {
        match &self.ruleset.exprs[expr] {
            Expr::Integer { .. } | Expr::Parameter(_) | Expr::Bool(_) | Expr::Const(_) => {
                indexset![expr]
            }
            Expr::Constructor { args, .. } => std::iter::chain(
                args.iter().map(|expr| self.decompose_expr(*expr)).flatten(),
                std::iter::once(expr),
            )
            .collect(),
            Expr::Extractor { arg, .. } => {
                std::iter::chain(self.decompose_expr(*arg), std::iter::once(expr)).collect()
            }
            Expr::MatchSome(expr) | Expr::MakeSome(expr) | Expr::TupleIndex { expr, .. } => {
                self.decompose_expr(*expr)
            }
        }
    }

    fn generate_decision(&mut self, decision: &Decision) -> std::fmt::Result {
        match decision {
            Decision::Return { bindings, expr } => {
                let exprs = std::iter::chain(
                    bindings
                        .iter()
                        .map(|expr| self.decompose_expr(*expr))
                        .flatten(),
                    self.decompose_expr(*expr),
                )
                .collect();

                self.generate_lets(&exprs, |generator| {
                    write_indended!(generator.buf, "return ")?;
                    generator.generate_expr(*expr)?;
                    writeln!(generator.buf, ";")
                })
            }
            Decision::Fail => {
                if self.module.decls[self.ruleset.decl].partial {
                    writeln_indended!(self.buf, "return None;")
                } else {
                    writeln_indended!(self.buf, "panic!(\"no rule matched\");")
                }
            }
            Decision::If {
                condition,
                then,
                otherwise,
            } => self.generate_lets(&condition.decompose(self), |generator| {
                write_indended!(generator.buf, "if ")?;

                let exprs_to_bind: &IndexSet<ExprIdx> = match condition {
                    Condition::Eq(lhs, rhs) => {
                        generator.generate_expr(*lhs)?;
                        write!(generator.buf, " == ")?;
                        generator.generate_expr(*rhs)?;

                        &IndexSet::new()
                    }
                    Condition::Some(matched, some) => {
                        write!(generator.buf, "let Some(")?;
                        generator.with_bound_exprs(&indexset![*matched], |generator| {
                            generator.generate_expr(*matched)
                        })?;
                        write!(generator.buf, ") = ")?;
                        generator.generate_expr(*some)?;

                        &indexset![*matched]
                    }
                };

                writeln!(generator.buf, " {{")?;

                generator.with_bound_exprs(exprs_to_bind, |generator| {
                    generator.with_indent(|generator| generator.generate_decision(then))
                })?;

                if let Some(otherwise) = otherwise {
                    writeln_indended!(generator.buf, "}} else {{")?;
                    generator.with_indent(|generator| generator.generate_decision(otherwise))?;
                }

                writeln_indended!(generator.buf, "}}")
            }),
            Decision::Let { expr, decision } => {
                let exprs = std::iter::chain(
                    self.decompose_expr(*expr).iter().cloned(),
                    std::iter::once(*expr),
                )
                .collect();

                self.generate_lets(&exprs, |generator| generator.generate_decision(decision))
            }
            Decision::Sequence(decisions) => {
                for decision in decisions {
                    self.generate_decision(decision)?;
                }

                Ok(())
            }
        }
    }

    fn generate_expr(&mut self, expr: ExprIdx) -> std::fmt::Result {
        if self.bound_exprs.contains(&expr) {
            return write!(self.buf, "v{}", expr.index());
        }

        match &self.ruleset.exprs[expr] {
            Expr::Integer { value, ty: _ } => write!(self.buf, "{value}"),
            Expr::Parameter(idx) => write!(self.buf, "p{idx}"),
            Expr::Bool(value) => write!(self.buf, "{value}"),
            Expr::Const(const_) => write!(self.buf, "{}", self.module.consts[*const_].name),
            Expr::Constructor { decl, args } => {
                let decl = &self.module.decls[*decl];
                let ctor = &decl.constructor;
                let ctor_expr = match ctor {
                    Some(ctor) => format!("C::{}", ctor.name),
                    None => format!("{}_ctor", decl.name),
                };
                write!(self.buf, "{ctor_expr}(ctx, ")?;

                let mut iter = args.iter().peekable();
                while let Some(expr) = iter.next() {
                    self.generate_expr(*expr)?;

                    if iter.peek().is_some() {
                        write!(self.buf, ", ")?;
                    }
                }

                write!(self.buf, ")")
            }
            Expr::Extractor { decl, arg } => {
                let decl = &self.module.decls[*decl];
                let etor = &decl.extractor;
                let etor_expr = match etor {
                    Some(etor) => format!("C::{}", etor.name),
                    None => format!("{}_etor", decl.name),
                };
                write!(self.buf, "{etor_expr}(ctx, ")?;
                self.generate_expr(*arg)?;

                write!(self.buf, ")")
            }
            Expr::MatchSome(expr) => {
                self.generate_expr(*expr)?;

                write!(self.buf, "?")
            }
            Expr::MakeSome(expr) => {
                write!(self.buf, "Some(")?;
                self.generate_expr(*expr)?;

                write!(self.buf, ")")
            }
            Expr::TupleIndex { expr, idx } => {
                self.generate_expr(*expr)?;

                write!(self.buf, ".{idx}")
            }
        }
    }
}

impl Condition {
    fn decompose<W: Write>(&self, generator: &RuleSetGenerator<W>) -> IndexSet<ExprIdx> {
        match self {
            Self::Eq(lhs, rhs) => std::iter::chain(
                generator.decompose_expr(*lhs),
                generator.decompose_expr(*rhs),
            )
            .collect(),
            Self::Some(lhs, rhs) => std::iter::chain(
                generator.decompose_expr(*lhs),
                generator.decompose_expr(*rhs),
            )
            .collect(),
        }
    }
}

fn all_paths_return_value(decision: &Decision) -> bool {
    match decision {
        Decision::If {
            then, otherwise, ..
        } => {
            all_paths_return_value(then)
                && otherwise
                    .as_ref()
                    .map(|decision| all_paths_return_value(decision))
                    .unwrap_or_default()
        }
        Decision::Let { decision, .. } => all_paths_return_value(decision),
        Decision::Return { .. } | Decision::Fail => true,
        Decision::Sequence(decisions) => decisions
            .iter()
            .any(|decision| all_paths_return_value(decision)),
    }
}

pub fn generate(module: Module) -> Result<String, std::fmt::Error> {
    let mut buf = String::new();
    let mut indent_buf = IndentBuffer::new(&mut buf);
    let mut generator = ModuleGenerator::new(&module, &mut indent_buf);

    generator.generate_context_trait()?;

    for ruleset in &module.rulesets {
        generator.generate_ruleset(ruleset)?;
    }

    Ok(buf)
}
