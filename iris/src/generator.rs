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

#[derive(Clone, Copy, Default)]
pub struct VariableIdx(usize);

impl std::ops::AddAssign<usize> for VariableIdx {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl std::fmt::Display for VariableIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.0)
    }
}

struct BodyEnv<'a> {
    pat_env: &'a HashMap<String, OccurrenceIdx>,
    env: HashMap<String, VariableIdx>,
}

impl<'a> BodyEnv<'a> {
    fn new(pat_env: &'a HashMap<String, OccurrenceIdx>) -> Self {
        Self {
            pat_env,
            env: HashMap::new(),
        }
    }

    fn add(&mut self, name: String, var_idx: VariableIdx) {
        assert!(self.env.insert(name, var_idx).is_none());
    }

    fn get<F: FnOnce(OccurrenceIdx) -> VariableIdx>(&self, name: &str, map: F) -> VariableIdx {
        self.env
            .get(name)
            .copied()
            .unwrap_or_else(|| map(self.pat_env[name]))
    }
}

struct Generator<'a, W: Write> {
    module: &'a Module,
    buf: &'a mut W,
    indent: usize,
    next_var_idx: VariableIdx,
    occurrences: IndexVec<OccurrenceIdx, VariableIdx>,
    stack: Vec<OccurrenceIdx>,
}

impl<'a, W: Write> Generator<'a, W> {
    fn new(module: &'a Module, buf: &'a mut W) -> Self {
        Self {
            module,
            buf,
            indent: 0,
            next_var_idx: VariableIdx::default(),
            occurrences: IndexVec::new(),
            stack: Vec::new(),
        }
    }

    fn create_var(&mut self) -> VariableIdx {
        let idx = self.next_var_idx;

        self.next_var_idx += 1;

        idx
    }

    fn push_new_occurences_to_stack(&mut self, count: usize) {
        for idx in 0..count {
            let var_idx = self.create_var();
            let occurrence_idx = self.occurrences.push(var_idx);

            self.stack.insert(idx, occurrence_idx);
        }
    }

    fn replace_first_stack_var_with(&mut self, count: usize) -> OccurrenceIdx {
        let occurrence_idx = self.stack.remove(0);

        self.push_new_occurences_to_stack(count);

        occurrence_idx
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
        actions: &mut IndexVec<ActionIdx, Action>,
        ctor_name: &str,
    ) -> std::fmt::Result {
        match decision {
            Decision::Leaf(action_idx) => {
                let Action { env, body } = &actions[action_idx];
                let mut env = BodyEnv::new(env);

                for stmt in &body.lets {
                    let var_idx = self.create_var();

                    write_indended!(self, "let {} = ", var_idx);
                    self.generate_expr(&env, &stmt.value)?;

                    if let Expr::Call { name, .. } = &stmt.value
                        && self.module.decls[name].partial
                    {
                        assert!(
                            self.module.decls[ctor_name].partial,
                            "can't call partial constructor in non-partial constructor"
                        );

                        write!(self.buf, "?")?;
                    }

                    writeln!(self.buf, ";")?;

                    env.add(stmt.name.clone(), var_idx);
                }

                let wrap_in_some = matches!(
                    &body.expr,
                    Expr::Call { name, .. } if !self.module.decls[name].partial
                ) && self.module.decls[ctor_name].partial;

                write_indended!(self, "return ");

                if wrap_in_some {
                    write!(self.buf, "Some(")?;
                }

                self.generate_expr(&env, &body.expr)?;

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
                        Constructor::Int(value) => writeln!(
                            self.buf,
                            "{} == {value} {{",
                            self.occurrences[self.stack[0]]
                        )?,
                        Constructor::True => {
                            writeln!(self.buf, "{} {{", self.occurrences[self.stack[0]])?
                        }
                        Constructor::False => {
                            writeln!(self.buf, "!{} {{", self.occurrences[self.stack[0]])?
                        }
                        Constructor::External(name) => {
                            let decl = &self.module.decls[&name];
                            let arity = decl.arg_tys.len();
                            let occurrence_idx = self.replace_first_stack_var_with(arity);
                            let vars: Vec<_> = self
                                .stack
                                .iter()
                                .take(arity)
                                .map(|&idx| self.occurrences[idx].to_string())
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
                                vars, etor_expr, self.occurrences[self.stack[0]]
                            )?;

                            if is_infallible {
                                writeln!(self.buf, ";")?;
                                writeln_indended!(self, "{{");
                            } else {
                                writeln!(self.buf, " {{")?;
                            }
                        }
                        Constructor::Pinned(name) => {
                            writeln!(self.buf, "{} == {name} {{", self.occurrences[self.stack[0]])?
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
            Decision::Guard(expr, action_idx, decision) => {
                self.push_new_occurences_to_stack(1);

                let var_idx = self.occurrences[self.stack[0]];
                let env = &actions[action_idx].env;

                if let Expr::Call { name, .. } = &expr
                    && self.module.decls[name].partial
                {
                    assert!(
                        self.module.decls[ctor_name].partial,
                        "can't call partial constructor in non-partial constructor"
                    );

                    write_indended!(self, "if let Some({}) = ", var_idx);
                    self.generate_expr(&BodyEnv::new(&env), &expr)?;
                    writeln!(self.buf, "{{")?;
                } else {
                    write_indended!(self, "let {} = ", var_idx);
                    self.generate_expr(&BodyEnv::new(&env), &expr)?;
                    writeln!(self.buf, ";")?;
                    writeln_indended!(self, "{{");
                }

                self.indended(|generator| {
                    generator.generate_decision(*decision, actions, ctor_name)
                })?;
                self.stack.remove(0);
                writeln_indended!(self, "}}");

                Ok(())
            }
            Decision::Sequence(decisions) => {
                let mut iter = decisions.into_iter().peekable();

                while let Some(decision) = iter.next() {
                    self.generate_decision(decision, actions, ctor_name)?;

                    if iter.peek().is_some() {
                        writeln!(self.buf)?;
                    }
                }

                Ok(())
            }
        }
    }

    fn generate_ctor(&mut self, name: &str) -> std::fmt::Result {
        self.next_var_idx = VariableIdx::default();
        self.occurrences.clear();
        self.stack.clear();

        let decl = &self.module.decls[name];
        let arity = decl.arg_tys.len();

        self.push_new_occurences_to_stack(arity);

        let params = self
            .stack
            .iter()
            .enumerate()
            .take(arity)
            .map(|(idx, &occurrence_idx)| {
                format!(
                    "{}: {}",
                    self.occurrences[occurrence_idx], decl.arg_tys[idx]
                )
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
                let mut _match =
                    decision::compile(&generator.module, lhs, &decl.arg_tys, !is_last_iteration);

                if is_last_iteration {
                    generate_unreachable = has_switch_without_default_case(&_match.tree);
                }

                generator.generate_decision(_match.tree, &mut _match.actions, name)?;
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

    fn generate_expr(&mut self, env: &BodyEnv, expr: &Expr) -> std::fmt::Result {
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
            Expr::Ident(name) => write!(
                self.buf,
                "{}",
                env.get(name, |occurrence_idx| self.occurrences[occurrence_idx])
            ),
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
        Decision::Guard(_, _, decision) => has_switch_without_default_case(decision),
        Decision::Sequence(decisions) => decisions
            .iter()
            .any(|decision| has_switch_without_default_case(decision)),
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
