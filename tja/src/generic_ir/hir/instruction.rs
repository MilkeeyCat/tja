use crate::hir::{
    BlockId, Constant, Function, TargetInstruction, TyStorage, Value, module::Declarations,
};
use slotmap::new_key_type;
use std::{collections::BTreeMap, fmt::Display};

new_key_type! {
    pub(crate) struct InstructionId;
}

pub(crate) enum Instruction<TI: TargetInstruction> {
    Const { const_: Constant },
    Target(TI),
}

impl<TI: TargetInstruction> Instruction<TI> {
    fn name(&self) -> &'static str {
        match self {
            Self::Const { .. } => "const",
            Self::Target(instr) => instr.name(),
        }
    }
}

pub(crate) enum Terminator {
    Return(Option<Value>),
}

impl Terminator {
    fn name(&self) -> &'static str {
        match self {
            Self::Return { .. } => "ret",
        }
    }
}

pub(super) struct InstrsIter<'a, TI: TargetInstruction> {
    func: &'a Function<TI>,
    next: Option<InstructionId>,
}

impl<'a, TI: TargetInstruction> InstrsIter<'a, TI> {
    pub(super) fn new(func: &'a Function<TI>, start: Option<InstructionId>) -> Self {
        Self { func, next: start }
    }
}

impl<TI: TargetInstruction> Iterator for InstrsIter<'_, TI> {
    type Item = InstructionId;

    fn next(&mut self) -> Option<Self::Item> {
        let instr = self.next?;

        self.next = self.func.next_instr(instr);

        Some(instr)
    }
}

pub(crate) struct DisplayInstr<'a, TI: TargetInstruction> {
    decls: &'a Declarations,
    func: &'a Function<TI>,
    ty_storage: &'a TyStorage,
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    instr: InstructionId,
}

impl<'a, TI: TargetInstruction> DisplayInstr<'a, TI> {
    pub(super) fn new(
        decls: &'a Declarations,
        func: &'a Function<TI>,
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

impl<TI: TargetInstruction> Display for DisplayInstr<'_, TI> {
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
            Instruction::Target(instr) => instr.fmt(self, f),
        };

        Ok(())
    }
}

pub(super) struct DisplayTerminator<'a, TI: TargetInstruction> {
    func: &'a Function<TI>,
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    block: BlockId,
}

impl<'a, TI: TargetInstruction> DisplayTerminator<'a, TI> {
    pub(super) fn new(
        func: &'a Function<TI>,
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

impl<TI: TargetInstruction> Display for DisplayTerminator<'_, TI> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let terminator = self.func.block(self.block).terminator();

        write!(f, "{}", terminator.name())?;

        match terminator {
            Terminator::Return(value) => {
                if let Some(value) = value {
                    write!(f, " {}", value.display(self.instr_to_idx))?;
                }
            }
        };

        Ok(())
    }
}
