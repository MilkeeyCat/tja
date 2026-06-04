use crate::{
    Immediate,
    lir::{BlockId, Function, GlobalValueIdx, Value, module::Declarations},
};
use slotmap::new_key_type;
use smallvec::SmallVec;
use std::{collections::BTreeMap, fmt::Display};

new_key_type! {
    pub(super) struct InstructionId;
}

pub(super) enum Instruction {
    Iconst { imm: Immediate },
    Load { ptr: Value },
    Store { ptr: Value, value: Value },
    Shl { value: Value, bits: Value },
    Lshr { value: Value, bits: Value },
    GlobalValuePtr { value: GlobalValueIdx },
    Zext { value: Value },
    Trunc { value: Value },
}

impl Instruction {
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
        }
    }
}

pub(super) enum Terminator {
    Return(SmallVec<[Value; 1]>),
}

impl Terminator {
    fn name(&self) -> &'static str {
        match self {
            Self::Return(..) => "ret",
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
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    instr: InstructionId,
}

impl<'a> DisplayInstr<'a> {
    pub(super) fn new(
        decls: &'a Declarations,
        func: &'a Function,
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

impl Display for DisplayInstr<'_> {
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
