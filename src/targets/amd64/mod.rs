mod abi;
mod address_mode;
mod asm_printer;
pub mod emit_binary;
mod instruction_selector;
mod lower_stack_slots;
mod materialize_copy;
mod opcode;
mod prolog_epilog_inserter;
pub mod register;
mod register_class_selector;
mod sysv_calling_convention;

use crate::{
    codegen::allocator::Allocator,
    hir,
    mir::{
        self, BasicBlockPatch, FrameIdx, InstrBuilder, InstructionIdx, Operand, VregIdx,
        passes::two_address::TwoAddressForm,
    },
    pass::FunctionToModuleAdapter,
    targets::amd64::{
        address_mode::{AddressMode, Base},
        instruction_selector::InstructionSelection,
        lower_stack_slots::StackSlotsLowerer,
        materialize_copy::MaterializeCopy,
        opcode::{get_load_op, get_store_op},
        prolog_epilog_inserter::PrologEpilogInserter,
        register_class_selector::SelectRegisterClass,
    },
};
use abi::SysVAmd64;
pub use asm_printer::AsmPrinter;
use derive_more::Display;
pub use opcode::Opcode;
pub use register::Register;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum OperandKind {
    Register,
    Memory,
    Immediate,
}

impl From<&Operand> for OperandKind {
    fn from(value: &Operand) -> Self {
        match value {
            Operand::Register(_, _) => Self::Register,
            Operand::Frame(_) => Self::Memory,
            Operand::Immediate(_) => Self::Immediate,
            Operand::Global(_) | Operand::Function(_) | Operand::Block(_) => unimplemented!(),
        }
    }
}

// The terms "above" and "below" are associated with the CF flag and refer to
// the relationship between two unsigned integer values. The terms "greater" and
// "less" are associated with the SF and OF flags and refer to the relationship
// between two signed integer values.

#[derive(Debug, Display, Eq, PartialEq)]
#[repr(usize)]
pub enum Condition {
    #[display("a")]
    Above,
    #[display("ae")]
    AboveEqual,
    #[display("b")]
    Below,
    #[display("be")]
    BelowEqual,
    #[display("e")]
    Equal,
    #[display("ne")]
    NotEqual,
    #[display("g")]
    Greater,
    #[display("ge")]
    GreaterEqual,
    #[display("l")]
    Less,
    #[display("le")]
    LessEqual,

    Num,
}

impl From<usize> for Condition {
    fn from(value: usize) -> Self {
        assert!(value < Self::Num as usize);

        unsafe { std::mem::transmute::<_, Self>(value) }
    }
}

#[repr(usize)]
pub enum RegisterClass {
    Gpr64,
    Gpr32,
    Gpr16,
    Gpr8,
}

impl Into<mir::RegisterClass> for RegisterClass {
    fn into(self) -> mir::RegisterClass {
        mir::RegisterClass(self as usize)
    }
}

pub struct RegisterInfo(HashMap<mir::RegisterClass, Vec<mir::PhysicalRegister>>);

impl RegisterInfo {
    fn new() -> Self {
        Self(HashMap::from([
            (
                RegisterClass::Gpr64.into(),
                vec![
                    Register::R15.into(),
                    Register::R14.into(),
                    Register::R13.into(),
                    Register::R12.into(),
                    Register::R11.into(),
                    Register::R10.into(),
                    Register::R9.into(),
                    Register::R8.into(),
                    Register::Rbx.into(),
                    Register::Rcx.into(),
                    Register::Rdx.into(),
                    Register::Rsi.into(),
                    Register::Rdi.into(),
                    Register::Rax.into(),
                ],
            ),
            (
                RegisterClass::Gpr32.into(),
                vec![
                    Register::R15d.into(),
                    Register::R14d.into(),
                    Register::R13d.into(),
                    Register::R12d.into(),
                    Register::R11d.into(),
                    Register::R10d.into(),
                    Register::R9d.into(),
                    Register::R8d.into(),
                    Register::Ebx.into(),
                    Register::Ecx.into(),
                    Register::Edx.into(),
                    Register::Esi.into(),
                    Register::Edi.into(),
                    Register::Eax.into(),
                ],
            ),
            (
                RegisterClass::Gpr16.into(),
                vec![
                    Register::R15w.into(),
                    Register::R14w.into(),
                    Register::R13w.into(),
                    Register::R12w.into(),
                    Register::R11w.into(),
                    Register::R10w.into(),
                    Register::R9w.into(),
                    Register::R8w.into(),
                    Register::Bx.into(),
                    Register::Cx.into(),
                    Register::Dx.into(),
                    Register::Si.into(),
                    Register::Di.into(),
                    Register::Ax.into(),
                ],
            ),
            (
                RegisterClass::Gpr8.into(),
                vec![
                    Register::R15b.into(),
                    Register::R14b.into(),
                    Register::R13b.into(),
                    Register::R12b.into(),
                    Register::R11b.into(),
                    Register::R10b.into(),
                    Register::R9b.into(),
                    Register::R8b.into(),
                    Register::Bh.into(),
                    Register::Bl.into(),
                    Register::Cl.into(),
                    Register::Dl.into(),
                    Register::Sil.into(),
                    Register::Dil.into(),
                    Register::Ah.into(),
                    Register::Al.into(),
                ],
            ),
        ]))
    }
}

impl super::RegisterInfo for RegisterInfo {
    fn get_registers_by_class(&self, class: &mir::RegisterClass) -> &[mir::PhysicalRegister] {
        &self.0[class]
    }

    fn overlaps(&self, a: &mir::PhysicalRegister, b: &mir::PhysicalRegister) -> bool {
        let a: Register = (*a).into();
        let b: Register = (*b).into();

        a == b || a.contains(b) || b.contains(a)
    }

    fn get_name(&self, r: &mir::PhysicalRegister) -> &'static str {
        let r: Register = (*r).into();

        r.name()
    }

    fn get_register_size(&self, r: &mir::PhysicalRegister) -> usize {
        let r: Register = (*r).into();

        r.size()
    }
}

pub struct Target {
    register_info: RegisterInfo,
    abi: SysVAmd64,
}

impl Target {
    pub fn new() -> Self {
        Self {
            register_info: RegisterInfo::new(),
            abi: SysVAmd64::new(),
        }
    }

    pub fn add_hir_passes(&self, pass_manager: &mut hir::pass::ModulePassManager<'_, '_, Self>) {
        pass_manager.add_pass(hir::passes::lower::LowerFunctionToModuleAdapter::default());
    }

    pub fn add_mir_passes<'a>(
        &self,
        pass_manager: &mut mir::pass::ModulePassManager<'a, 'a, Self>,
    ) {
        let mut fn_pass_manager = mir::pass::FunctionPassManager::<'_, '_, Self>::new();

        fn_pass_manager.add_pass(SelectRegisterClass::default());
        fn_pass_manager.add_pass(InstructionSelection::default());
        fn_pass_manager.add_pass(TwoAddressForm::default());
        fn_pass_manager.add_pass(MaterializeCopy::default());
        fn_pass_manager.add_pass(Allocator::default());
        fn_pass_manager.add_pass(PrologEpilogInserter::default());
        fn_pass_manager.add_pass(StackSlotsLowerer::default());

        pass_manager.add_pass(FunctionToModuleAdapter::new(fn_pass_manager));
    }
}

impl super::Target for Target {
    type RegisterInfo = RegisterInfo;
    type Abi = SysVAmd64;

    fn register_info(&self) -> &Self::RegisterInfo {
        &self.register_info
    }

    fn abi(&self) -> &Self::Abi {
        &self.abi
    }

    fn store_reg_to_stack_slot(
        &self,
        patch: &mut BasicBlockPatch,
        idx: InstructionIdx,
        vreg_idx: VregIdx,
        frame_idx: FrameIdx,
        size: usize,
    ) {
        patch.add_instruction(
            idx,
            InstrBuilder::new(get_store_op(size).into())
                .add_addr_mode(AddressMode {
                    base: Base::Frame(frame_idx),
                    index: None,
                    scale: 1,
                    displacement: None,
                })
                .add_use(mir::Register::Virtual(vreg_idx))
                .into(),
        );
    }

    fn load_reg_from_stack_slot(
        &self,
        patch: &mut BasicBlockPatch,
        idx: InstructionIdx,
        vreg_idx: VregIdx,
        frame_idx: FrameIdx,
        size: usize,
    ) {
        patch.add_instruction(
            idx,
            InstrBuilder::new(get_load_op(size).into())
                .add_def(mir::Register::Virtual(vreg_idx))
                .add_addr_mode(AddressMode {
                    base: Base::Frame(frame_idx),
                    index: None,
                    scale: 1,
                    displacement: None,
                })
                .into(),
        );
    }

    fn is_move_op(&self, opcode: mir::Opcode) -> bool {
        match Opcode::from(opcode) {
            Opcode::Mov8rr | Opcode::Mov16rr | Opcode::Mov32rr | Opcode::Mov64rr => true,
            _ => false,
        }
    }
}

impl InstrBuilder {
    pub fn add_addr_mode(mut self, addr_mode: AddressMode) -> Self {
        let len = self.0.operands.len();

        addr_mode.write(&mut self.0.operands, len);

        self
    }
}
