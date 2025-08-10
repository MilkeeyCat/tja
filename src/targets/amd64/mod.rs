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
    pass::FunctionToModuleAdaptor,
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

pub struct RegisterInfo(HashMap<mir::RegisterClass, Vec<mir::PhysicalRegister>>);

impl RegisterInfo {
    fn new() -> Self {
        Self(HashMap::from([
            (
                RegisterClass::Gpr64 as mir::RegisterClass,
                vec![
                    Register::R15 as mir::PhysicalRegister,
                    Register::R14 as mir::PhysicalRegister,
                    Register::R13 as mir::PhysicalRegister,
                    Register::R12 as mir::PhysicalRegister,
                    Register::R11 as mir::PhysicalRegister,
                    Register::R10 as mir::PhysicalRegister,
                    Register::R9 as mir::PhysicalRegister,
                    Register::Rcx as mir::PhysicalRegister,
                    Register::Rdx as mir::PhysicalRegister,
                    Register::Rsi as mir::PhysicalRegister,
                    Register::Rdi as mir::PhysicalRegister,
                    Register::Rax as mir::PhysicalRegister,
                    //Register::R8 as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr32 as mir::RegisterClass,
                vec![
                    Register::R15d as mir::PhysicalRegister,
                    Register::R14d as mir::PhysicalRegister,
                    Register::R13d as mir::PhysicalRegister,
                    Register::R12d as mir::PhysicalRegister,
                    Register::R11d as mir::PhysicalRegister,
                    Register::R10d as mir::PhysicalRegister,
                    Register::R9d as mir::PhysicalRegister,
                    Register::Ecx as mir::PhysicalRegister,
                    Register::Edx as mir::PhysicalRegister,
                    Register::Esi as mir::PhysicalRegister,
                    Register::Edi as mir::PhysicalRegister,
                    Register::Eax as mir::PhysicalRegister,
                    //Register::R8d as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr16 as mir::RegisterClass,
                vec![
                    Register::R15w as mir::PhysicalRegister,
                    Register::R14w as mir::PhysicalRegister,
                    Register::R13w as mir::PhysicalRegister,
                    Register::R12w as mir::PhysicalRegister,
                    Register::R11w as mir::PhysicalRegister,
                    Register::R10w as mir::PhysicalRegister,
                    Register::R9w as mir::PhysicalRegister,
                    Register::Cx as mir::PhysicalRegister,
                    Register::Dx as mir::PhysicalRegister,
                    Register::Si as mir::PhysicalRegister,
                    Register::Di as mir::PhysicalRegister,
                    Register::Ax as mir::PhysicalRegister,
                    //Register::R8w as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr8 as mir::RegisterClass,
                vec![
                    Register::R15b as mir::PhysicalRegister,
                    Register::R14b as mir::PhysicalRegister,
                    Register::R13b as mir::PhysicalRegister,
                    Register::R12b as mir::PhysicalRegister,
                    Register::R11b as mir::PhysicalRegister,
                    Register::R10b as mir::PhysicalRegister,
                    Register::R9b as mir::PhysicalRegister,
                    Register::Cl as mir::PhysicalRegister,
                    Register::Dl as mir::PhysicalRegister,
                    Register::Sil as mir::PhysicalRegister,
                    Register::Dil as mir::PhysicalRegister,
                    Register::Ah as mir::PhysicalRegister,
                    Register::Al as mir::PhysicalRegister,
                    //Register::R8b as mir::Register,
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
        pass_manager.add_pass(hir::passes::lower::LowerFunctionToModuleAdaptor::default());
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

        pass_manager.add_pass(FunctionToModuleAdaptor::new(fn_pass_manager));
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
            InstrBuilder::new(get_store_op(size) as mir::Opcode)
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
            InstrBuilder::new(get_load_op(size) as mir::Opcode)
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
