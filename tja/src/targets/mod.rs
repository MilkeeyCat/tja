use crate::{
    hir::{self, passes::lower::FnLowering},
    mir::{self, Function, Instruction, VregIdx},
    ty::{self, TyIdx},
};

pub mod amd64;

pub trait Target {
    type Abi: Abi;
    // FIXME: it's not really calling convention, think of a better name
    type CallingConventionInstruction: Instruction;

    fn get_calling_convention(&self) -> &dyn CallingConvention<Target = Self>;

    fn lower_operand(
        &self,
        operand: &hir::Operand,
        types: &[TyIdx],
        func: &mut FnLowering<Self>,
    ) -> Vec<mir::VregIdx>
    where
        Self: Sized;
}

pub trait CallingConvention {
    type Target: Target;

    fn lower_ret(
        &self,
        lowering: &mut FnLowering<Self::Target>,
        operand: Option<(Vec<VregIdx>, TyIdx)>,
    );
    fn lower_params(
        &self,
        lowering: &mut FnLowering<Self::Target>,
        vreg_indices: Vec<Vec<VregIdx>>,
        tys: Vec<TyIdx>,
        ret_ty: TyIdx,
    );
    fn lower_call(
        &self,
        lowering: &mut FnLowering<Self::Target>,
        callee_vreg_idx: VregIdx,
        arg_vreg_indices: Vec<Vec<VregIdx>>,
        arg_tys: Vec<TyIdx>,
        ret: Option<(Vec<VregIdx>, TyIdx)>,
    );
}

pub trait Abi {
    fn field_offset(storage: &ty::Storage, fields: &[TyIdx], i: usize) -> usize;
    fn ty_size(storage: &ty::Storage, ty: TyIdx) -> usize;
    fn alignment(storage: &ty::Storage, ty: TyIdx) -> usize;
}

pub trait RegisterClass: Copy {}

pub trait Register {
    type RegisterClass: RegisterClass;

    fn class<I: Instruction<Register = impl Register<RegisterClass = Self::RegisterClass>>>(
        &self,
        func: &Function<I>,
    ) -> Self::RegisterClass;
}
