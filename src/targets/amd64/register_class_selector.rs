use super::RegisterClass;
use crate::{
    mir::{Function, Operand, Register, RegisterRole},
    pass::{Context, Pass},
    targets::Target,
    ty::Ty,
};

#[derive(Default)]
pub struct SelectRegisterClass;

impl<'a, T: Target> Pass<'a, Function, T> for SelectRegisterClass {
    fn run(&self, func: &mut Function, ctx: &mut Context<'a, T>) {
        for block in &func.blocks {
            for instr in &block.instructions {
                for operand in &instr.operands {
                    if let Operand::Register(Register::Virtual(idx), RegisterRole::Def) = operand {
                        let class = match ctx.ty_storage.get_ty(func.vreg_info.get_vreg(*idx).ty) {
                            Ty::Void => unreachable!(),
                            Ty::I8 => RegisterClass::Gpr8,
                            Ty::I16 => RegisterClass::Gpr16,
                            Ty::I32 => RegisterClass::Gpr32,
                            Ty::I64 => RegisterClass::Gpr64,
                            Ty::Ptr => RegisterClass::Gpr64,
                            Ty::Struct(_) | Ty::Array { .. } => {
                                unreachable!("aggregate type should've been already lowered")
                            }
                        };

                        func.vreg_info.set_class(*idx, class.into());
                    }
                }
            }
        }
    }
}
