pub(crate) mod basic_block;
mod function;
pub(crate) mod instruction;
pub(crate) mod module;
pub(crate) mod signature;
mod ty;

pub(crate) use basic_block::Builder as BlockBuilder;
use basic_block::{Block, BlockId};
use derive_more::From;
pub(crate) use function::Builder as FunctionBuilder;
use function::Function;
pub(crate) use instruction::{Instruction, InstructionId, Terminator};
pub(crate) use module::{
    Builder as ModuleBuilder, FunctionDeclaration, GlobalVariableDeclaration, Module,
};
pub(crate) use signature::{ParamRanges, Signature};
pub(crate) use ty::Ty;

use crate::{FunctionIdx, GlobalVariableIdx, Immediate, generic_ir::target_instrs};
use instruction::DisplayInstr;
use std::{collections::BTreeMap, fmt::Display};

pub(crate) trait TargetInstruction: target_instrs::InstrName + Sized {
    fn fmt(&self, ctx: &DisplayInstr<Self>, f: &mut std::fmt::Formatter<'_>);
    fn result_tys(&self, ty: Option<Ty>) -> Vec<Ty>;
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Value {
    Param {
        ty: Ty,
        block: BlockId,
        idx: usize,
    },
    Instr {
        ty: Ty,
        instr: InstructionId,
        result_idx: usize,
    },
}

impl Value {
    pub(crate) fn ty(&self) -> Ty {
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

pub(crate) struct DisplayValue<'a> {
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

pub(crate) enum GlobalVariableValue {
    GlobalVariable(GlobalVariableIdx),
    Function(FunctionIdx),
    Imm(Ty, Immediate),
}

pub(crate) enum GlobalVariable {
    Zero(usize),
    Value(Vec<(usize, GlobalVariableValue)>),
}

pub(crate) struct DisplayGlobalVariable<'a, TI: TargetInstruction> {
    module: &'a Module<TI>,
    var: GlobalVariableIdx,
}

impl<TI: TargetInstruction> Display for DisplayGlobalVariable<'_, TI> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decl = &self.module.decls.global_vars[self.var];
        let global = self.module.global_vars.get(&self.var);

        write!(f, "{} = ", decl.name)?;

        if global.is_none() {
            write!(f, "external ")?;
        }

        write!(f, "global")?;

        if let Some(var) = global {
            match var {
                GlobalVariable::Zero(size) => write!(f, " zero({})", size)?,
                GlobalVariable::Value(values) => {
                    writeln!(f, " {{")?;

                    for (offset, value) in values {
                        write!(f, "\t{}: ", *offset)?;

                        match value {
                            GlobalVariableValue::GlobalVariable(var) => {
                                write!(f, "{}", self.module.decls.global_vars[*var].name)?;
                            }
                            GlobalVariableValue::Function(func) => {
                                write!(f, "{}", self.module.decls.funcs[*func].name)?;
                            }
                            GlobalVariableValue::Imm(ty, imm) => {
                                write!(f, "{} {}", ty, imm)?;
                            }
                        };

                        writeln!(f, ",")?;
                    }

                    writeln!(f, "}}")?;
                }
            };
        }

        Ok(())
    }
}

#[derive(Clone, Copy, From)]
pub(crate) enum GlobalValueIdx {
    GlobalValue(GlobalVariableIdx),
    Function(FunctionIdx),
}
