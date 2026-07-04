use crate::{
    Immediate,
    lir::{BlockId, Function, GlobalValueIdx, TargetInstruction, Value, module::Declarations},
};
use slotmap::new_key_type;
use smallvec::SmallVec;
use std::{collections::BTreeMap, fmt::Display};

new_key_type! {
    pub(crate) struct InstructionId;
}

pub(crate) enum Instruction<TI: TargetInstruction> {
    Iconst { imm: Immediate },
    Load { ptr: Value },
    Store { ptr: Value, value: Value },
    Shl { value: Value, bits: Value },
    Lshr { value: Value, bits: Value },
    GlobalValuePtr { value: GlobalValueIdx },
    Zext { value: Value },
    Trunc { value: Value },
    PtrAdd { ptr: Value, offset: Value },
    IntToPtr { int: Value },
    PtrToInt { ptr: Value },
    Or { lhs: Value, rhs: Value },
    Target(TI),
}

impl<TI: TargetInstruction> Instruction<TI> {
    fn name(&self) -> &'static str {
        match self {
            Self::Iconst { .. } => "iconst",
            Self::Load { .. } => "load",
            Self::Store { .. } => "store",
            Self::Shl { .. } => "shl",
            Self::Lshr { .. } => "lshr",
            Self::GlobalValuePtr { .. } => "global_value_ptr",
            Self::Zext { .. } => "zext",
            Self::Trunc { .. } => "trunc",
            Self::PtrAdd { .. } => "ptr_add",
            Self::IntToPtr { .. } => "int_to_ptr",
            Self::PtrToInt { .. } => "ptr_to_int",
            Self::Or { .. } => "or",
            Self::Target(instr) => instr.name(),
        }
    }
}

pub(crate) enum Terminator {
    Return(SmallVec<[Value; 1]>),
}

impl Terminator {
    fn name(&self) -> &'static str {
        match self {
            Self::Return(..) => "ret",
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
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    instr: InstructionId,
}

impl<'a, TI: TargetInstruction> DisplayInstr<'a, TI> {
    pub(super) fn new(
        decls: &'a Declarations,
        func: &'a Function<TI>,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
        instr: InstructionId,
    ) -> Self {
        Self {
            decls,
            func,
            instr,
            instr_to_idx,
        }
    }
}

impl<TI: TargetInstruction> Display for DisplayInstr<'_, TI> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.func.instr_results(self.instr).iter().peekable();
        let has_results = iter.peek().is_some();

        while let Some(value) = iter.next() {
            write!(f, "{}", value.display(self.instr_to_idx))?;

            if iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        if has_results {
            write!(f, " = ")?;
        }

        let instr = self.func.instr(self.instr);

        write!(f, "{}", instr.name())?;

        match instr {
            Instruction::Iconst { imm } => {
                let ty = self.func.instr_results(self.instr)[0].ty();

                write!(f, " {}, {}", imm, ty)?;
            }
            Instruction::Load { ptr } => {
                let ty = self.func.instr_results(self.instr)[0].ty();

                write!(f, " {}, {}", ptr.display(self.instr_to_idx), ty)?;
            }
            Instruction::Store { ptr, value } => {
                write!(
                    f,
                    " {}, {}",
                    ptr.display(self.instr_to_idx),
                    value.display(self.instr_to_idx)
                )?;
            }
            Instruction::Shl { value, bits } | Instruction::Lshr { value, bits } => {
                write!(
                    f,
                    " {}, {}",
                    value.display(self.instr_to_idx),
                    bits.display(self.instr_to_idx)
                )?;
            }
            &Instruction::GlobalValuePtr { value } => {
                write!(
                    f,
                    " {}",
                    match value {
                        GlobalValueIdx::GlobalValue(var) => &self.decls.global_vars[var].name,
                        GlobalValueIdx::Function(func) => &self.decls.funcs[func].name,
                    }
                )?;
            }
            Instruction::Zext { value } | Instruction::Trunc { value } => {
                let ty = self.func.instr_results(self.instr)[0].ty();

                write!(f, " {}, {}", value.display(self.instr_to_idx), ty)?;
            }
            Instruction::PtrAdd { ptr, offset } => {
                write!(
                    f,
                    " {}, {}",
                    ptr.display(self.instr_to_idx),
                    offset.display(self.instr_to_idx)
                )?;
            }
            Instruction::IntToPtr { int } => {
                write!(f, " {}", int.display(self.instr_to_idx))?;
            }
            Instruction::PtrToInt { ptr } => {
                let ty = self.func.instr_results(self.instr)[0].ty();

                write!(f, " {}, {}", ptr.display(self.instr_to_idx), ty)?;
            }
            Instruction::Or { lhs, rhs } => {
                write!(
                    f,
                    " {}, {}",
                    lhs.display(self.instr_to_idx),
                    rhs.display(self.instr_to_idx),
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
            Terminator::Return(values) => {
                if !values.is_empty() {
                    write!(f, " ")?;
                }

                let mut iter = values.iter().peekable();

                while let Some(value) = iter.next() {
                    write!(f, "{}", value.display(self.instr_to_idx))?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
            }
        };

        Ok(())
    }
}
