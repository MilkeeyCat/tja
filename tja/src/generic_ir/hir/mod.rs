pub(crate) mod basic_block;
pub(crate) mod constant;
mod function;
pub(crate) mod instruction;
mod lower;
mod module;
mod ty;

use basic_block::Block;
pub use basic_block::{BlockId, Builder as BlockBuilder};
pub use constant::Constant;
use function::Function;
pub use function::{Builder as FunctionBuilder, Signature};
pub(crate) use instruction::{Instruction, InstructionId, Terminator};
pub(crate) use lower::{FuncLoweringCtx, lower};
pub use module::{Builder as ModuleBuilder, Module};
pub use ty::{Storage as TyStorage, Ty, TyIdx};

use crate::{FunctionIdx, GlobalVariableIdx, generic_ir::target_instrs, lir};
use instruction::DisplayInstr;
use module::Declarations;
use std::{collections::BTreeMap, fmt::Display};

#[allow(private_bounds, private_interfaces)]
pub trait TargetInstruction: target_instrs::InstrName + Sized {
    type LirTargetInstr: lir::TargetInstruction;

    fn fmt(&self, ctx: &DisplayInstr<Self>, f: &mut std::fmt::Formatter<'_>);
    fn result_tys(&self, ty_storage: &mut TyStorage, ty: Option<TyIdx>) -> Vec<TyIdx>;
    fn lower(&self, ctx: &mut FuncLoweringCtx<'_, Self>, instr: InstructionId);
}

pub enum GlobalVariable {
    Zero,
    Const(Constant),
}

pub struct DisplayGlobalVariable<'a, TI: TargetInstruction> {
    module: &'a Module<TI>,
    ty_storage: &'a TyStorage,
    var: GlobalVariableIdx,
}

impl<TI: TargetInstruction> Display for DisplayGlobalVariable<'_, TI> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decl = self.module.decls.global_var(self.var);
        let global = self.module.global_vars.get(&self.var);

        write!(f, "{} = ", decl.name)?;

        if global.is_none() {
            write!(f, "external ")?;
        }

        write!(f, "global {}", decl.ty.display(self.ty_storage))?;

        if let Some(var) = global {
            write!(f, ", ")?;

            match var {
                GlobalVariable::Zero => write!(f, "zero")?,
                GlobalVariable::Const(const_) => {
                    write!(f, "{}", const_.display(&self.module.decls))?
                }
            };
        }

        Ok(())
    }
}

#[allow(private_interfaces)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    Param {
        ty: TyIdx,
        block: BlockId,
        idx: usize,
    },
    Instr {
        ty: TyIdx,
        instr: InstructionId,
        result_idx: usize,
    },
}

impl Value {
    pub fn ty(&self) -> TyIdx {
        match self {
            Self::Param { ty, .. } => *ty,
            Self::Instr { ty, .. } => *ty,
        }
    }

    fn display<'a>(&'a self, instr_to_idx: &'a BTreeMap<InstructionId, usize>) -> DisplayValue<'a> {
        DisplayValue {
            instr_to_idx,
            value: self,
        }
    }
}

pub struct DisplayValue<'a> {
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    value: &'a Value,
}

impl Display for DisplayValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Param { idx, .. } => write!(f, "p{}", idx),
            Value::Instr {
                instr, result_idx, ..
            } => write!(f, "v{}_{}", self.instr_to_idx[instr], result_idx),
        }
    }
}
