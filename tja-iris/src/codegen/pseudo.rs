use crate::{
    TermDeclIdx,
    ast::GlobalKind,
    codegen::{
        fmt::{Generator, IndentWriter, gen_comma_separated},
        scopes::Scopes,
    },
    late::{
        Block, Constraint, ControlFlow, EvalStep, ExprIdx, ExprKind, Program, Type,
        TypeCardinality, TypeIdx,
    },
    sema::{self, GlobalIdx, Literal},
    with_indent, write_indented, writeln_indented,
};
use std::fmt::{Result, Write};

struct ProgramGenerator<'a, W: Write> {
    program: &'a Program,
    writer: IndentWriter<W>,
}

impl<'a, W: Write> ProgramGenerator<'a, W> {
    fn new(program: &'a Program, writer: W) -> Self {
        Self {
            program,
            writer: IndentWriter::new(writer),
        }
    }

    fn gen_sema_type(&mut self, ty: sema::TypeIdx) -> Result {
        match &self.program.sema_types[ty] {
            sema::Type::Builtin(ty) => write!(self.writer, "{}", ty),
            sema::Type::External(name) => write!(self.writer, "{}", name),
            sema::Type::Tuple(tys) => {
                write!(self.writer, "(")?;
                gen_comma_separated(self, tys, Self::gen_sema_type)?;
                write!(self.writer, ")")
            }
        }
    }

    fn gen_late_type(&mut self, ty: TypeIdx) -> Result {
        let Type { ty, cardinality } = self.program.late_types[ty];

        match cardinality {
            TypeCardinality::Option => {
                write!(self.writer, "Option<")?;
                self.gen_sema_type(ty)?;
                write!(self.writer, ">")
            }
            TypeCardinality::Scalar => self.gen_sema_type(ty),
            TypeCardinality::Iterator => {
                write!(self.writer, "Iterator<")?;
                self.gen_sema_type(ty)?;
                write!(self.writer, ">")
            }
        }
    }

    fn gen_term_name(&mut self, decl: TermDeclIdx) -> Result {
        let decl = &self.program.term_decls[decl];

        write!(
            self.writer,
            "{}{}",
            if decl.is_external { "@" } else { "" },
            decl.name
        )
    }

    fn gen_term_signature(&mut self, decl_idx: TermDeclIdx) -> Result {
        let decl = &self.program.term_decls[decl_idx];

        if decl.is_external {
            write!(self.writer, "extern ")?;
        }

        write!(self.writer, "term ")?;
        self.gen_term_name(decl_idx)?;
        write!(self.writer, "(")?;

        for (idx, &ty) in decl.arg_tys.iter().enumerate() {
            if idx > 0 {
                write!(self.writer, ", ")?;
            }

            if !decl.is_external {
                self.gen_param(idx)?;
                write!(self.writer, ": ")?;
            }

            self.gen_sema_type(ty)?;
        }

        write!(self.writer, ") -> ")?;
        self.gen_late_type(decl.ret_ty)
    }

    fn gen_extern_terms(&mut self) -> Result {
        let mut gen_nl = false;

        for (idx, decl) in self.program.term_decls.iter_enumerated() {
            if decl.is_external {
                gen_nl = true;

                self.gen_term_signature(idx)?;
                writeln!(self.writer, ";")?;
            }
        }

        if gen_nl {
            writeln!(self.writer)?;
        }

        Ok(())
    }

    fn gen_globals(&mut self) -> Result {
        for (idx, global) in self.program.globals.iter_enumerated() {
            write!(self.writer, "extern let ")?;
            self.gen_global(idx)?;
            write!(self.writer, ": ")?;
            self.gen_sema_type(global.ty)?;
            writeln!(self.writer, ";")?;
        }

        if !self.program.globals.is_empty() {
            writeln!(self.writer)?;
        }

        Ok(())
    }

    fn gen_param(&mut self, idx: usize) -> Result {
        write!(self.writer, "p{}", idx)
    }

    fn gen_global(&mut self, global: GlobalIdx) -> Result {
        let global = &self.program.globals[global];
        let prefix = match global.kind {
            GlobalKind::Const => "$",
            GlobalKind::Field => "%",
        };

        write!(self.writer, "{}{}", prefix, global.name)
    }

    fn gen_term(&mut self, decl: TermDeclIdx) -> Result {
        self.gen_term_signature(decl)?;
        writeln!(self.writer, " {{")?;
        TermBodyGenerator::new(self, decl).generate()?;
        writeln!(self.writer, "}}")
    }

    fn generate(mut self) -> Result {
        self.gen_extern_terms()?;
        self.gen_globals()?;

        for &decl in self.program.terms.keys() {
            self.gen_term(decl)?;
        }

        Ok(())
    }
}

impl<W: Write> Generator for ProgramGenerator<'_, W> {
    type Writer = IndentWriter<W>;

    fn writer(&mut self) -> &mut Self::Writer {
        &mut self.writer
    }
}

struct TermBodyGenerator<'a, 'b, W: Write> {
    parent: &'a mut ProgramGenerator<'b, W>,
    scopes: Scopes,
    decl: TermDeclIdx,
}

impl<'a, 'b, W: Write> TermBodyGenerator<'a, 'b, W> {
    fn new(parent: &'a mut ProgramGenerator<'b, W>, decl: TermDeclIdx) -> Self {
        Self {
            parent,
            scopes: Scopes::new(),
            decl,
        }
    }

    fn generate(mut self) -> Result {
        self.gen_block(&self.parent.program.terms[&self.decl])
    }

    fn gen_block(&mut self, block: &Block) -> Result {
        with_indent!(self.parent.writer, {
            for step in &block.0 {
                self.gen_step(step)?;
            }
        });

        Ok(())
    }

    fn enter_scope_with_bindings(&mut self, bindings: &[ExprIdx]) {
        self.scopes.enter();

        for &expr in bindings {
            self.scopes.bind(expr);
        }
    }

    fn gen_step(&mut self, step: &EvalStep) -> Result {
        for &expr in &step.bindings {
            write_indented!(self.parent.writer, "let ")?;
            self.gen_variable(self.scopes.next_var_idx())?;
            write!(self.parent.writer, ": ")?;
            self.parent
                .gen_late_type(self.parent.program.exprs[expr].ty)?;
            write!(self.parent.writer, " = ")?;
            self.gen_expr(expr)?;
            self.scopes.bind(expr);
            writeln!(self.parent.writer, ";")?;
        }

        match step.ctrl_flow {
            ControlFlow::Match { target, ref arms } => {
                write_indented!(self.parent.writer, "match ")?;
                self.gen_expr(target)?;
                writeln!(self.parent.writer, " {{")?;
                with_indent!(self.parent.writer, {
                    for arm in arms {
                        self.enter_scope_with_bindings(&arm.bindings);
                        write_indented!(self.parent.writer, "")?;

                        match arm.constraint {
                            Constraint::Bool(value) => write!(self.parent.writer, "{}", value),
                            Constraint::Global(global) => self.parent.gen_global(global),
                            Constraint::Int(value) => write!(self.parent.writer, "{}", value),
                            Constraint::Local(expr) => self.gen_expr(expr),
                            Constraint::Some => write!(self.parent.writer, "Some"),
                        }?;

                        if !arm.bindings.is_empty() {
                            write!(self.parent.writer, "(")?;
                            gen_comma_separated(self, &arm.bindings, Self::gen_expr)?;
                            write!(self.parent.writer, ")")?;
                        }

                        writeln!(self.parent.writer, " => {{")?;
                        self.gen_block(&arm.body)?;
                        writeln_indented!(self.parent.writer, "}}")?;
                        self.scopes.leave();
                    }
                });
                writeln_indented!(self.parent.writer, "}}")
            }
            ControlFlow::Loop {
                item,
                iterator,
                ref body,
            } => {
                self.enter_scope_with_bindings(&[item]);
                write_indented!(self.parent.writer, "for ")?;
                self.gen_expr(item)?;
                write!(self.parent.writer, " in ")?;
                self.gen_expr(iterator)?;
                writeln!(self.parent.writer, " {{")?;
                self.gen_block(body)?;
                writeln_indented!(self.parent.writer, "}}")?;
                self.scopes.leave();

                Ok(())
            }
            ControlFlow::Return(expr) => {
                let is_multi = self.parent.program.late_types
                    [self.parent.program.term_decls[self.decl].ret_ty]
                    .cardinality
                    == TypeCardinality::Iterator;

                write_indented!(
                    self.parent.writer,
                    "{} ",
                    if is_multi { "yield" } else { "return" }
                )?;
                self.gen_expr(expr)?;
                writeln!(self.parent.writer, ";")
            }
            ControlFlow::Bail => {
                writeln_indented!(self.parent.writer, "bail;")
            }
        }
    }

    fn gen_expr(&mut self, expr: ExprIdx) -> Result {
        if let Some(idx) = self.scopes.get(expr) {
            return self.gen_variable(idx);
        }

        match self.parent.program.exprs[expr].kind {
            ExprKind::Call {
                term_decl,
                ref args,
            } => {
                self.parent.gen_term_name(term_decl)?;
                write!(self.parent.writer, "(")?;
                gen_comma_separated(self, args, Self::gen_expr)?;
                write!(self.parent.writer, ")")
            }
            ExprKind::Global(global) => self.parent.gen_global(global),
            ExprKind::Literal(Literal::Int(value)) => write!(self.parent.writer, "{}", value),
            ExprKind::Literal(Literal::Bool(value)) => write!(self.parent.writer, "{}", value),
            ExprKind::Parameter(idx) => self.parent.gen_param(idx),
            ExprKind::TupleIndex { expr, idx } => {
                self.gen_expr(expr)?;
                write!(self.parent.writer, ".{}", idx)
            }
            ExprKind::Tuple(ref exprs) => {
                write!(self.parent.writer, "(")?;
                gen_comma_separated(self, exprs, Self::gen_expr)?;
                write!(self.parent.writer, ")")
            }
            ExprKind::Next(expr) => {
                self.gen_expr(expr)?;
                write!(self.parent.writer, ".next()")
            }
            ExprKind::MatchSome(_) => unreachable!(),
            ExprKind::MakeSome(expr) => {
                write!(self.parent.writer, "Some(")?;
                self.gen_expr(expr)?;
                write!(self.parent.writer, ")")
            }
        }
    }

    fn gen_variable(&mut self, idx: usize) -> Result {
        write!(self.parent.writer, "v{}", idx)
    }
}

impl<W: Write> Generator for TermBodyGenerator<'_, '_, W> {
    type Writer = IndentWriter<W>;

    fn writer(&mut self) -> &mut Self::Writer {
        &mut self.parent.writer
    }
}

pub(crate) fn generate<W: Write>(program: &Program, writer: W) -> Result {
    ProgramGenerator::new(program, writer).generate()
}
