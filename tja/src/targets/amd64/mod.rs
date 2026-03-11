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
    ) -> Option<Self::RegisterClass> {
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
}
