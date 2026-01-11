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
                if let Some(name) = &decl.constructor {
                    let params = decl
                        .arg_tys
                        .iter()
                        .enumerate()
                        .map(|(idx, ty)| format!("arg{idx}: {ty}"))
                        .collect::<Vec<_>>()
                        .join(", ");

                    writeln_indended!(
                        generator,
                        "fn {name}(&mut self, {}) -> {};",
                        params,
                        decl.ret_ty,
                    );
                }

                if let Some(name) = &decl.extractor {
                    let ret_tys: Vec<_> = decl.arg_tys.iter().map(|ty| ty.to_string()).collect();
                    let ret_tys = if ret_tys.len() > 1 {
                        format!("({})", ret_tys.join(", "))
                    } else {
                        ret_tys.join(", ")
                    };

                    writeln_indended!(
                        generator,
                        "fn {name}(&mut self, arg0: {}) -> Option<{}>;",
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
    ) -> std::fmt::Result {
        match decision {
            Decision::Leaf(action_idx) => {
                let Action { env, expr } = &actions[action_idx];

                write_indended!(self, "");
                self.generate_expr(env, expr)?;
                writeln!(self.buf, "")
            }
            Decision::Fail => {
                writeln_indended!(self, "panic!(\"no rule matched\");");

                Ok(())
            }
            Decision::Switch(cases, default) => {
                for (ctor, decision) in cases {
                    write_indended!(self, "if ");

                    match ctor {
                        Constructor::Int(value) => {
                            write!(self.buf, "{} == {value}", self.variables[self.stack[0]])?
                        }
                        Constructor::True => write!(self.buf, "{}", self.variables[self.stack[0]])?,
                        Constructor::False => {
                            write!(self.buf, "!{}", self.variables[self.stack[0]])?
                        }
                        Constructor::External(name) => {
                            let arity = self.module.decls[&name].arg_tys.len();
                            let occurrence_idx = self.stack.remove(0);

                            self.create_vars(arity);

                            let vars = self
                                .stack
                                .iter()
                                .take(arity)
                                .map(|&idx| self.variables[idx].as_str())
                                .collect::<Vec<_>>()
                                .join(", ");
                            let etor_expr = match &self.module.decls[&name].extractor {
                                Some(name) => format!("C::{name}"),
                                None => format!("{name}_etor"),
                            };

                            self.stack.insert(0, occurrence_idx);
                            write!(
                                self.buf,
                                "let Some({}) = {}(ctx, {})",
                                vars, etor_expr, self.variables[self.stack[0]]
                            )?;
                        }
                        Constructor::Pinned(name) => {
                            write!(self.buf, "{} == {name}", self.variables[self.stack[0]])?;
                        }
                    }

                    self.stack.remove(0);

                    writeln!(self.buf, " {{")?;
                    self.indended(|generator| generator.generate_decision(decision, actions))?;
                    write_indended!(self, "}}");
                }

                if let Some(default) = default {
                    writeln!(self.buf, " else {{")?;
                    self.indended(|generator| generator.generate_decision(*default, actions))?;
                    write_indended!(self, "}}");
                }

                writeln!(self.buf, "")
            }
            Decision::Swap(occurrence_idx, decision) => {
                self.stack.swap(0, occurrence_idx.raw());

                self.generate_decision(*decision, actions)
            }
        }
    }

    fn generate_ctor(&mut self, name: &str) -> std::fmt::Result {
        self.variables.clear();
        self.stack.clear();

        let decl = &self.module.decls[name];
        let arity = decl.arg_tys.len();
        let _match = decision::compile(&self.module, &self.module.rules[name], &decl.arg_tys);

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

        writeln!(
            self.buf,
            "fn {name}_ctor<C: Context>(ctx: &mut C, {params}) -> {} {{",
            decl.ret_ty,
        )?;

        self.indended(|generator| generator.generate_decision(_match.tree, &_match.actions))?;

        writeln!(self.buf, "}}")
    }

    fn generate_expr(
        &mut self,
        env: &HashMap<String, OccurrenceIdx>,
        expr: &Expr,
    ) -> std::fmt::Result {
        match expr {
            Expr::Call { name, args } => {
                let ctor_expr = match &self.module.decls[name].constructor {
                    Some(name) => format!("C::{name}"),
                    None => format!("{name}_ctor"),
                };
                write!(self.buf, "{ctor_expr}(ctx, ")?;

                let mut iter = args.iter().peekable();

                while let Some(expr) = iter.next() {
                    self.generate_expr(env, expr)?;

                    if iter.peek().is_some() {
                        write!(self.buf, ", ")?;
                    }
                }

                write!(self.buf, ")")
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

pub fn generate(module: Module) -> Result<String, std::fmt::Error> {
    let mut buf = String::new();
    let mut generator = Generator::new(&module, &mut buf);

    generator.generate_context_trait()?;

    for (name, _) in &module.rules {
        generator.generate_ctor(name)?;
    }

    Ok(buf)
}
