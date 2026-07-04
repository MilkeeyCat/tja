use crate::{
    hir::{self, TyStorage},
    lir,
};

pub struct Instruction;

impl super::InstrName for Instruction {
    fn name(&self) -> &'static str {
        unreachable!()
    }
}

#[allow(private_interfaces)]
impl hir::TargetInstruction for Instruction {
    type LirTargetInstr = Self;

    fn fmt(&self, _ctx: &hir::instruction::DisplayInstr<Self>, _f: &mut std::fmt::Formatter<'_>) {
        unreachable!()
    }

    fn result_tys(&self, _ty_storage: &mut TyStorage, _ty: Option<hir::TyIdx>) -> Vec<hir::TyIdx> {
        unreachable!()
    }

    fn lower(&self, _ctx: &mut hir::FuncLoweringCtx<'_, Self>, _instr: hir::InstructionId) {
        unreachable!()
    }
}

impl lir::TargetInstruction for Instruction {
    fn fmt(&self, _ctx: &lir::instruction::DisplayInstr<Self>, _f: &mut std::fmt::Formatter<'_>) {
        unreachable!()
    }

    fn result_tys(&self, _ty: Option<lir::Ty>) -> Vec<lir::Ty> {
        unreachable!()
    }
}
