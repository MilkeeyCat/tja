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
        if func.is_declaration() {
            return;
        }

        let mut bb_cursor = func.block_cursor_mut();

        while let Some(bb_idx) = bb_cursor.move_next() {
            let mut instr_cursor = bb_cursor.func.instr_cursor_mut(bb_idx);

            while let Some(instr_idx) = instr_cursor.move_next() {
                let instr = instr_cursor.func.instructions.get_mut(instr_idx).unwrap();

                for operand in &instr.operands {
                    if let Operand::Register(Register::Virtual(idx), RegisterRole::Def) = operand {
                        let class = match ctx
                            .ty_storage
                            .get_ty(instr_cursor.func.vreg_info.get_vreg(*idx).ty)
                        {
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

                        instr_cursor.func.vreg_info.set_class(*idx, class.into());
                    }
                }
            }
        }
    }
}
