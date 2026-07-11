pub mod amd64;
mod basic_block;
mod function;
mod instruction;
mod module;

use basic_block::{Block, BlockId};
use function::Function;
pub(crate) use instruction::Instruction;
use instruction::InstructionId;
pub(crate) use module::Module;

use crate::{
    hir::{self, FuncLoweringCtx, TargetInstruction, TyStorage},
    lir::{self, ParamRanges},
};
use index_vec::define_index_type;

#[allow(private_interfaces, private_bounds)]
pub trait Target {
    type TargetInstruction: TargetInstruction;
    type Instr: Instruction;

    fn abi(&self) -> &dyn Abi<TargetInstruction = Self::TargetInstruction>;
}

pub(crate) trait Abi {
    type TargetInstruction: TargetInstruction;

    fn field_offset(&self, ty_storage: &TyStorage, fields: &[hir::TyIdx], idx: usize) -> usize;
    fn hir_ty_size(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize;
    fn lir_ty_size(&self, ty: lir::Ty) -> usize;
    fn alignment(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize;

    fn calling_conv(&self) -> &dyn CallingConvention<TargetInstruction = Self::TargetInstruction>;
}

pub(crate) trait CallingConvention {
    type TargetInstruction: TargetInstruction;

    fn lower_signature(
        &self,
        abi: &dyn Abi<TargetInstruction = Self::TargetInstruction>,
        ty_storage: &TyStorage,
        sig: &hir::Signature,
    ) -> (lir::Signature, ParamRanges);
    fn lower_entry_block_params(
        &self,
        ctx: &mut FuncLoweringCtx<'_, Self::TargetInstruction>,
        hir_params: &[hir::Value],
        lir_params: &[(lir::signature::Value, lir::Value)],
        param_ranges: &ParamRanges,
    );
    fn lower_ret(
        &self,
        ctx: &mut FuncLoweringCtx<'_, Self::TargetInstruction>,
        value: Option<(hir::Value, &[lir::signature::Value])>,
    );
}

define_index_type! {
    struct VregIdx = usize;
}

pub(crate) trait PhysicalRegister: std::fmt::Display {}

pub(crate) enum Register<PR: PhysicalRegister> {
    Physical(PR),
    Virtual(VregIdx),
}
