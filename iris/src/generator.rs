use crate::{
    ast::{Expr, Literal},
    decision::{self, Action, ActionIdx, Constructor, Decision, OccurrenceIdx},
    lower::Module,
};
use index_vec::IndexVec;
use std::{collections::HashMap, fmt::Write};

macro_rules! write_indended {
    ($self:expr, $($arg:tt)*) => {
        write!($self.buf, "{}", " ".repeat($self.indent))?;
        write!($self.buf, $($arg)*)?;
    };
}

macro_rules! writeln_indended {
    ($self:expr $(,)?) => {
        write!($self.buf, "{}\n", " ".repeat($self.indent))?;
    };
    ($self:expr, $($arg:tt)*) => {
        write!($self.buf, "{}", " ".repeat($self.indent))?;
        writeln!($self.buf, $($arg)*)?;
    };
}

struct Generator<'a, W: Write> {
    module: &'a Module,
    buf: &'a mut W,
    indent: usize,
    variables: IndexVec<OccurrenceIdx, String>,
    stack: Vec<OccurrenceIdx>,
}

impl<'a, W: Write> Generator<'a, W> {
    fn new(module: &'a Module, buf: &'a mut W) -> Self {
        Self {
            module,
            buf,
            indent: 0,
            variables: IndexVec::new(),
            stack: Vec::new(),
        }
    }

    fn create_vars(&mut self, n: usize) {
        for i in 0..n {
            let idx = self.variables.push(format!("v{}", self.variables.len()));

            self.stack.insert(i, idx);
        }
    }

    fn indended<F: FnOnce(&mut Generator<W>) -> std::fmt::Result>(
        &mut self,
        f: F,
    ) -> std::fmt::Result {
        self.indent += 4;
        f(self)?;
        self.indent -= 4;

        Ok(())
    }

    fn generate_context_trait(&mut self) -> std::fmt::Result {
        writeln!(self.buf, "pub trait Context {{")?;

        self.indended(|generator| {
            for (_, decl) in &self.module.decls {
                if let Some(ctor) = &decl.constructor {
                    let params = decl
                        .arg_tys
                        .iter()
                        .enumerate()
                        .map(|(idx, ty)| format!("arg{idx}: {ty}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let ret_ty = if decl.partial {
                        format!("Option<{}>", decl.ret_ty)
                    } else {
                        decl.ret_ty.to_string()
                    };

                    writeln_indended!(
                        generator,
                        "fn {}(&mut self, {}) -> {};",
                        ctor.name,
                        params,
                        ret_ty,
                    );
                }

                if let Some(etor) = &decl.extractor {
                    let ret_tys: Vec<_> = decl.arg_tys.iter().map(|ty| ty.to_string()).collect();
                    let mut ret_tys = if ret_tys.len() > 1 {
                        format!("({})", ret_tys.join(", "))
                    } else {
                        ret_tys.join(", ")
                    };

                    if !etor.infallible {
                        ret_tys = format!("Option<{}>", ret_tys);
                    }

                    writeln_indended!(
                        generator,
                        "fn {}(&mut self, arg0: {}) -> {};",
                        etor.name,
                        decl.ret_ty,
                        ret_tys,
                    );
                }
            }

            Ok(())
        })?;

        writeln!(self.buf, "}}")?;
        writeln!(self.buf, "")
    }

    fn generate_decision(
        &mut self,
        decision: Decision,
        actions: &IndexVec<ActionIdx, Action>,
        ctor_name: &str,
    ) -> std::fmt::Result {
        match decision {
            Decision::Leaf(action_idx) => {
                let Action { env, expr } = &actions[action_idx];
                let wrap_in_some = !matches!(
                    expr,
                    Expr::Call { name, .. } if !self.module.decls[name].partial
                ) && self.module.decls[ctor_name].partial;

                write_indended!(self, "return ");

                if wrap_in_some {
                    write!(self.buf, "Some(")?;
                }

                self.generate_expr(env, expr)?;

                if wrap_in_some {
                    write!(self.buf, ")")?;
                }

                writeln!(self.buf, ";")
            }
            Decision::Fail => {
                if self.module.decls[ctor_name].partial {
                    writeln_indended!(self, "return None;");
                } else {
                    writeln_indended!(self, "panic!(\"no rule matched\");");
                }

                Ok(())
            }
            Decision::Switch(cases, default) => {
                let mut if_expr = false;
                let mut iter = cases.into_iter().enumerate();

                while let Some((idx, (ctor, decision))) = iter.next() {
                    if_expr = !matches!(
                        &ctor,
                        Constructor::External(name)
                            if self.module.decls[name]
                                .extractor
                                .as_ref()
                                .unwrap().
                                infallible
                    );

                    if if_expr {
                        if idx > 0 {
                            write!(self.buf, " else if ")?;
                        } else {
                            write_indended!(self, "if ");
                        }
                    }

                    match ctor {
                        Constructor::Int(value) => {
                            writeln!(self.buf, "{} == {value} {{", self.variables[self.stack[0]])?
                        }
                        Constructor::True => {
                            writeln!(self.buf, "{} {{", self.variables[self.stack[0]])?
                        }
                        Constructor::False => {
                            writeln!(self.buf, "!{} {{", self.variables[self.stack[0]])?
                        }
                        Constructor::External(name) => {
                            let decl = &self.module.decls[&name];
                            let arity = decl.arg_tys.len();
                            let occurrence_idx = self.stack.remove(0);

                            self.create_vars(arity);

                            let vars: Vec<_> = self
                                .stack
                                .iter()
                                .take(arity)
                                .map(|&idx| self.variables[idx].as_str())
                                .collect();
                            let mut vars = if vars.len() > 1 {
                                format!("({})", vars.join(", "))
                            } else {
                                vars.join(", ")
                            };
                            let is_infallible = decl.extractor.as_ref().unwrap().infallible;

                            if !is_infallible {
                                vars = format!("Some({})", vars);
                            } else {
                                write_indended!(self, "");
                            }

                            let etor_expr = match &self.module.decls[&name].extractor {
                                Some(etor) => format!("C::{}", etor.name),
                                None => format!("{name}_etor"),
                            };

                            self.stack.insert(0, occurrence_idx);
                            write!(
                                self.buf,
                                "let {} = {}(ctx, {})",
                                vars, etor_expr, self.variables[self.stack[0]]
                            )?;

                            if is_infallible {
                                writeln!(self.buf, ";")?;
                                writeln_indended!(self, "{{");
                            } else {
                                writeln!(self.buf, " {{")?;
                            }
                        }
                        Constructor::Pinned(name) => {
                            writeln!(self.buf, "{} == {name} {{", self.variables[self.stack[0]])?
                        }
                    }

                    let occurrence_idx = self.stack.remove(0);

                    self.indended(|generator| {
                        generator.generate_decision(decision, actions, ctor_name)
                    })?;
                    self.stack.insert(0, occurrence_idx);
                    write_indended!(self, "}}");
                }

                if let Some(default) = default {
                    if if_expr {
                        writeln!(self.buf, " else {{")?;
                    } else {
                        writeln!(self.buf, "")?;
                        writeln_indended!(self, "{{");
                    }

                    self.indended(|generator| {
                        generator.generate_decision(*default, actions, ctor_name)
                    })?;
                    write_indended!(self, "}}");
                }

                writeln!(self.buf, "")
            }
            Decision::Swap(occurrence_idx, decision) => {
                self.stack.swap(0, occurrence_idx.raw());

                self.generate_decision(*decision, actions, ctor_name)
            }
        }
    }

    fn generate_ctor(&mut self, name: &str) -> std::fmt::Result {
        self.variables.clear();
        self.stack.clear();

        let decl = &self.module.decls[name];
        let arity = decl.arg_tys.len();

        self.create_vars(arity);

        let params = self
            .stack
            .iter()
            .enumerate()
            .take(arity)
            .map(|(idx, &occurrence_idx)| {
                format!("{}: {}", self.variables[occurrence_idx], decl.arg_tys[idx])
            })
            .collect::<Vec<_>>()
            .join(", ");
        let ret_ty = if decl.partial {
            format!("Option<{}>", decl.ret_ty)
        } else {
            decl.ret_ty.to_string()
        };

        writeln!(
            self.buf,
            "fn {name}_ctor<C: Context>(ctx: &mut C, {params}) -> {} {{",
            ret_ty
        )?;

        self.indended(|generator| {
            let mut rules = generator.module.rules[name].as_slice();
            let mut generate_unreachable = false;

            while !rules.is_empty() {
                let idx = rules
                    .windows(2)
                    .position(|rules| rules[0].priority != rules[1].priority)
                    .map(|idx| idx + 1)
                    .unwrap_or_else(|| rules.len());
                let (lhs, rhs) = rules.split_at(idx);
                let is_last_iteration = rhs.is_empty();
                let _match =
                    decision::compile(&generator.module, lhs, &decl.arg_tys, !is_last_iteration);

                if is_last_iteration {
                    generate_unreachable = has_switch_without_default_case(&_match.tree);
                }

                generator.generate_decision(_match.tree, &_match.actions, name)?;
                rules = rhs;
            }

            // if a switch is exhaustive over the constructors, there will be no
            // `else` clause generated and rust will cry about not all paths
            // returning a value
            if generate_unreachable {
                writeln!(generator.buf, "")?;
                writeln_indended!(generator, "unreachable!();");
            }

            Ok(())
        })?;

        writeln!(self.buf, "}}")
    }

    fn generate_expr(
        &mut self,
        env: &HashMap<String, OccurrenceIdx>,
        expr: &Expr,
    ) -> std::fmt::Result {
        match expr {
            Expr::Call { name, args } => {
                let ctor = &self.module.decls[name].constructor;
                let ctor_expr = match ctor {
                    Some(ctor) => format!("C::{}", ctor.name),
                    None => format!("{name}_ctor"),
                };
                write!(self.buf, "{ctor_expr}(ctx, ")?;

                let mut iter = args.iter().peekable();

                while let Some(expr) = iter.next() {
                    self.generate_expr(env, expr)?;

                    if let Expr::Call { name, .. } = expr
                        && self.module.decls[name].partial
                    {
                        write!(self.buf, "?")?;
                    }
                    if iter.peek().is_some() {
                        write!(self.buf, ", ")?;
                    }
                }

                write!(self.buf, ")")?;

                Ok(())
            }
            Expr::Literal(literal) => match literal {
                Literal::Int(value) => write!(self.buf, "{value}"),
                Literal::Bool(value) => write!(self.buf, "{value}"),
                Literal::Const(value) => write!(self.buf, "{value}"),
            },
            Expr::Ident(name) => write!(self.buf, "{}", self.variables[env[name]]),
        }
    }
}

fn has_switch_without_default_case(decision: &Decision) -> bool {
    match decision {
        Decision::Leaf(_) | Decision::Fail => false,
        Decision::Switch(cases, default) => {
            default.is_none()
                || cases
                    .iter()
                    .any(|(_, decision)| has_switch_without_default_case(decision))
        }
        Decision::Swap(_, decision) => has_switch_without_default_case(decision),
    }
}

pub fn generate(mut module: Module) -> Result<String, std::fmt::Error> {
    for (_, rules) in &mut module.rules {
        rules.sort_unstable_by_key(|rule| std::cmp::Reverse(rule.priority));
    }

    let mut buf = String::new();
    let mut generator = Generator::new(&module, &mut buf);

    generator.generate_context_trait()?;

    for (name, _) in &module.rules {
        generator.generate_ctor(name)?;
    }

    Ok(buf)
}
