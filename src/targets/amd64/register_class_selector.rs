use super::RegisterClass;
use crate::{
    hir::ty::{self, Ty},
    mir::{self, Mir, Operand, Register, RegisterRole},
};

pub fn select_register_class(mir: &mut Mir, ty_storage: &ty::Storage) {
    for module in &mut mir.0 {
        for func in &mut module.functions {
            for block in &func.blocks {
                for instr in &block.instructions {
                    for operand in &instr.operands {
                        match operand {
                            Operand::Register(Register::Virtual(idx), RegisterRole::Def) => {
                                let class = match ty_storage.get_ty(func.vreg_types[idx]) {
                                    Ty::Void => unreachable!(),
                                    Ty::I8 => RegisterClass::Gpr8,
                                    Ty::I16 => RegisterClass::Gpr16,
                                    Ty::I32 => RegisterClass::Gpr32,
                                    Ty::I64 => RegisterClass::Gpr64,
                                    Ty::Ptr => RegisterClass::Gpr64,
                                    Ty::Struct(_) => {
                                        unreachable!("struct type should've been already lowered")
                                    }
                                };

                                func.vreg_classes.insert(*idx, class as mir::RegisterClass);
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
    }
}
