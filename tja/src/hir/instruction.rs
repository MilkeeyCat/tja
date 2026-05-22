use crate::hir::{BlockId, Constant, Function, TyStorage, Value, module::Declarations};
use slotmap::new_key_type;
use smallvec::SmallVec;
use std::{collections::BTreeMap, fmt::Display};

new_key_type! {
    pub(super) struct InstructionId;
}

pub(super) enum Instruction {
    Const { const_: Constant },
}

impl Instruction {
    fn name(&self) -> &'static str {
        match self {
            Self::Const { .. } => "const",
        }
    }
}

pub(super) enum Terminator {
    Return(SmallVec<[Value; 1]>),
}

impl Terminator {
    fn name(&self) -> &'static str {
        match self {
            Self::Return { .. } => "ret",
        }
    }
}

pub(super) struct InstrsIter<'a> {
    func: &'a Function,
    next: Option<InstructionId>,
}

impl<'a> InstrsIter<'a> {
    pub(super) fn new(func: &'a Function, start: Option<InstructionId>) -> Self {
        Self { func, next: start }
    }
}

impl Iterator for InstrsIter<'_> {
    type Item = InstructionId;

    fn next(&mut self) -> Option<Self::Item> {
        let instr = self.next?;

        self.next = self.func.next_instr(instr);

        Some(instr)
    }
}

pub(super) struct DisplayInstr<'a> {
    decls: &'a Declarations,
    func: &'a Function,
    ty_storage: &'a TyStorage,
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    instr: InstructionId,
}

impl<'a> DisplayInstr<'a> {
    pub(super) fn new(
        decls: &'a Declarations,
        func: &'a Function,
        ty_storage: &'a TyStorage,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
        instr: InstructionId,
    ) -> Self {
        Self {
            decls,
            func,
            ty_storage,
            instr,
            instr_to_idx,
        }
    }
}

impl Display for DisplayInstr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.func.instr_results(self.instr).iter().peekable();

        while let Some(value) = iter.next() {
            write!(f, "{}", value.display(self.instr_to_idx))?;

            if iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        let instr = self.func.instr(self.instr);

        write!(f, " = {}", instr.name())?;

        match instr {
            Instruction::Const { const_ } => {
                let ty = self.func.instr_results(self.instr)[0].ty();

                write!(
                    f,
                    " {}, {}",
                    const_.display(self.decls),
                    ty.display(self.ty_storage)
                )?;
            }
        };

        Ok(())
    }
}

pub(super) struct DisplayTerminator<'a> {
    func: &'a Function,
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    block: BlockId,
}

impl<'a> DisplayTerminator<'a> {
    pub(super) fn new(
        func: &'a Function,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
        block: BlockId,
    ) -> Self {
        Self {
            func,
            instr_to_idx,
            block,
        }
    }
}

impl Display for DisplayTerminator<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let terminator = self.func.block(self.block).terminator();

        write!(f, "{}", terminator.name())?;

        match terminator {
            Terminator::Return(values) => {
                let mut iter = values.iter().peekable();

                while let Some(value) = iter.next() {
                    write!(f, " {}", value.display(self.instr_to_idx))?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
            }
        };

        Ok(())
    }
}
