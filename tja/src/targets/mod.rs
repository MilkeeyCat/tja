pub mod amd64;

use crate::hir::passes::lower::FnLowering;
use crate::mir::{
    FrameIdx, InstructionCursorMut, Opcode, PhysicalRegister, RegisterClass, VregIdx,
};
use crate::ty::{Storage, TyIdx};

pub trait RegisterInfo {
    fn get_registers_by_class(&self, class: &RegisterClass) -> &[PhysicalRegister];
    fn overlaps(&self, a: &PhysicalRegister, b: &PhysicalRegister) -> bool;
    fn get_name(&self, r: &PhysicalRegister) -> &'static str;
    fn get_register_size(&self, r: &PhysicalRegister) -> usize;
}

pub trait Target {
    type Abi: Abi;
    type RegisterInfo: RegisterInfo;

    fn abi(&self) -> &Self::Abi;
    fn register_info(&self) -> &Self::RegisterInfo;

    fn store_reg_to_stack_slot(
        &self,
        cursor: &mut InstructionCursorMut,
        vreg_idx: VregIdx,
        frame_idx: FrameIdx,
        size: usize,
    );
    fn load_reg_from_stack_slot(
        &self,
        cursor: &mut InstructionCursorMut,
        vreg_idx: VregIdx,
        frame_idx: FrameIdx,
        size: usize,
    );
    fn is_move_op(&self, opcode: Opcode) -> bool;
}

pub trait CallingConvention {
    fn lower_ret<A: Abi>(
        &self,
        lowering: &mut FnLowering<A>,
        operand: Option<(Vec<VregIdx>, TyIdx)>,
    );
    fn lower_params<A: Abi>(
        &self,
        lowering: &mut FnLowering<A>,
        vreg_indices: Vec<Vec<VregIdx>>,
        tys: Vec<TyIdx>,
        ret_ty: TyIdx,
    );
    fn lower_call<A: Abi>(
        &self,
        lowering: &mut FnLowering<A>,
        callee_vreg_idx: VregIdx,
        arg_vreg_indices: Vec<Vec<VregIdx>>,
        arg_tys: Vec<TyIdx>,
        ret: Option<(Vec<VregIdx>, TyIdx)>,
    );
}

pub trait Abi {
    type CallingConvention: CallingConvention;

    fn field_offset(&self, storage: &Storage, fields: &[TyIdx], i: usize) -> usize;
    fn ty_size(&self, storage: &Storage, ty: TyIdx) -> usize;
    fn alignment(&self, storage: &Storage, ty: TyIdx) -> usize;
    fn calling_convention(&self) -> &Self::CallingConvention;

    fn callee_saved_regs(&self) -> &'static [PhysicalRegister];
    fn caller_saved_regs(&self) -> &'static [PhysicalRegister];
}
