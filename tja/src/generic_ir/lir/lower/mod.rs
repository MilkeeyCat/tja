use crate::{
    hir, lir,
    mir::{self, Target},
};

pub(crate) fn lower<T: Target>(
    target: &T,
    module: lir::Module<<T::TargetInstruction as hir::TargetInstruction>::LirTargetInstr>,
) -> mir::Module<T::Instr> {
    todo!()
}
