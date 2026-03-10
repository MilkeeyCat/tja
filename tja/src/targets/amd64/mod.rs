mod abi;
mod calling_convention;

use crate::{
    Const,
    hir::{self, passes::lower::FnLowering},
    mir::{self, GenericInstruction, GenericRegister},
    ty::{self, Ty, TyIdx},
};

#[derive(Clone, Copy)]
pub enum RegisterClass {
    Gpr8,
    Gpr16,
    Gpr32,
    Gpr64,
}

impl super::RegisterClass for RegisterClass {}

pub enum Register {
    Rax,
    Eax,
    Ax,
    Ah,
    Al,
}

impl super::Register for Register {
    type RegisterClass = RegisterClass;

    fn class<
        I: super::Instruction<Register = impl super::Register<RegisterClass = Self::RegisterClass>>,
    >(
        &self,
        _func: &mir::Function<I>,
    ) -> Self::RegisterClass {
        todo!()
    }
}

pub enum Instruction<R: super::Register> {
    Foo,
    Bar(R),
}

impl<R: super::Register> mir::Instruction for Instruction<R> {
    type Register = R;
}

pub struct Target {
    default_cc: calling_convention::SysV,
}

impl Target {
    pub fn new() -> Self {
        Self {
            default_cc: calling_convention::SysV,
        }
    }
}

fn ty_to_reg_class(storage: &ty::Storage, ty: TyIdx) -> RegisterClass {
    match storage.get_ty(ty) {
        Ty::I8 => RegisterClass::Gpr8,
        Ty::I16 => RegisterClass::Gpr16,
        Ty::I32 => RegisterClass::Gpr32,
        Ty::I64 | Ty::Ptr => RegisterClass::Gpr64,
        _ => panic!("unexpected type"),
    }
}

impl super::Target for Target {
    type Abi = abi::SysV;
    type CallingConventionInstruction = GenericInstruction<Instruction<GenericRegister<Register>>>;

    fn get_calling_convention(&self) -> &dyn super::CallingConvention<Target = Self> {
        &self.default_cc
    }

    fn lower_operand(
        &self,
        operand: &hir::Operand,
        types: &[TyIdx],
        lowering: &mut FnLowering<Self>,
    ) -> Vec<mir::VregIdx> {
        let mut vreg_indices: Vec<mir::VregIdx> = Vec::new();

        match &operand {
            hir::Operand::Local(_) => {
                for &ty in types {
                    vreg_indices.push(
                        lowering
                            .mir_function
                            .vreg_info
                            .create_vreg(ty, ty_to_reg_class(lowering.ty_storage, ty)),
                    );
                }
            }
            hir::Operand::Const(c, ty) => {
                let reg_class = ty_to_reg_class(lowering.ty_storage, *ty);

                match c {
                    Const::Global(idx) => {
                        let vreg_idx = lowering.mir_function.vreg_info.create_vreg(*ty, reg_class);
                        let instr_idx =
                            lowering
                                .mir_function
                                .create_instr(GenericInstruction::GlobalValue {
                                    op0: vreg_idx.into(),
                                    op1: (*idx).into(),
                                });

                        lowering.instr_cursor_mut().insert_after(instr_idx);
                        vreg_indices.push(vreg_idx);
                    }
                    Const::Function(idx) => {
                        let vreg_idx = lowering.mir_function.vreg_info.create_vreg(*ty, reg_class);
                        let instr_idx =
                            lowering
                                .mir_function
                                .create_instr(GenericInstruction::GlobalValue {
                                    op0: vreg_idx.into(),
                                    op1: (*idx).into(),
                                });

                        lowering.instr_cursor_mut().insert_after(instr_idx);
                        vreg_indices.push(vreg_idx);
                    }
                    Const::Int(value) => {
                        let vreg_idx = lowering.mir_function.vreg_info.create_vreg(*ty, reg_class);
                        let instr_idx =
                            lowering
                                .mir_function
                                .create_instr(GenericInstruction::Copy {
                                    op0: vreg_idx.into(),
                                    op1: (*value as i64).into(),
                                });

                        lowering.instr_cursor_mut().insert_after(instr_idx);
                        vreg_indices.push(vreg_idx);
                    }
                    Const::Aggregate(consts) => {
                        match lowering.ty_storage.get_ty(*ty) {
                            Ty::Struct(tys) => {
                                assert_eq!(consts.len(), tys.len());

                                for (c, ty) in consts.iter().zip(tys) {
                                    let tmp = lowering
                                        .get_or_create_vregs(hir::Operand::Const(c.clone(), *ty));

                                    vreg_indices.extend_from_slice(tmp);
                                }
                            }
                            Ty::Array { ty, len } => {
                                assert_eq!(consts.len(), *len);

                                for c in consts.iter() {
                                    let tmp = lowering
                                        .get_or_create_vregs(hir::Operand::Const(c.clone(), *ty));

                                    vreg_indices.extend_from_slice(tmp);
                                }
                            }
                            _ => unreachable!(),
                        };
                    }
                }
            }
        }

        vreg_indices
    }
}
