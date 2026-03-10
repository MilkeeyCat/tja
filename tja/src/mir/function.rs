use crate::{
    mir::{
        BasicBlock, BasicBlockCursor, BasicBlockCursorMut, BlockIdx, InstructionCursor,
        InstructionCursorMut, InstructionIdx,
        instruction::{Instruction, InstructionWrapper},
    },
    targets::{Register, RegisterClass},
    ty::TyIdx,
};
use index_vec::{IndexVec, define_index_type};
use typed_generational_arena::StandardArena;

define_index_type! {
    pub struct VregIdx = usize;
}

define_index_type! {
    pub struct FrameIdx = usize;
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum GenericRegister<R: Register> {
    Virtual(VregIdx),
    Physical(R),
}

impl<R: Register> From<VregIdx> for GenericRegister<R> {
    fn from(vreg: VregIdx) -> Self {
        Self::Virtual(vreg)
    }
}

impl<R: Register> Register for GenericRegister<R> {
    type RegisterClass = R::RegisterClass;

    fn class<I: Instruction<Register = impl Register<RegisterClass = Self::RegisterClass>>>(
        &self,
        func: &Function<I>,
    ) -> Self::RegisterClass {
        match self {
            Self::Virtual(vreg) => func.vreg_info.get_vreg(*vreg).class,
            Self::Physical(reg) => reg.class(func),
        }
    }
}

pub struct Function<I: Instruction> {
    pub name: String,
    pub vreg_info: VregInfo<<I::Register as Register>::RegisterClass>,
    pub frame_info: FrameInfo,
    pub blocks: StandardArena<BasicBlock<I>>,
    pub instructions: StandardArena<InstructionWrapper<I>>,
    pub block_head: Option<BlockIdx<I>>,
    pub block_tail: Option<BlockIdx<I>>,
}

impl<I: Instruction> Function<I> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            vreg_info: VregInfo::new(),
            frame_info: FrameInfo::default(),
            blocks: StandardArena::new(),
            instructions: StandardArena::new(),
            block_head: None,
            block_tail: None,
        }
    }

    pub fn create_block(&mut self, name: String) -> BlockIdx<I> {
        self.blocks.insert(BasicBlock::new(name))
    }

    pub fn block_cursor(&self) -> BasicBlockCursor<'_, I> {
        BasicBlockCursor::new(self)
    }

    pub fn block_cursor_mut(&mut self) -> BasicBlockCursorMut<'_, I> {
        BasicBlockCursorMut::new(self)
    }

    pub fn instr_cursor(&self, idx: BlockIdx<I>) -> InstructionCursor<'_, I> {
        InstructionCursor::new(self, idx)
    }

    pub fn instr_cursor_mut(&mut self, idx: BlockIdx<I>) -> InstructionCursorMut<'_, I> {
        InstructionCursorMut::new(self, idx)
    }

    pub fn create_instr(&mut self, instr: impl Into<I>) -> InstructionIdx<I> {
        self.instructions.insert(InstructionWrapper {
            instruction: instr.into(),
            next: None,
            prev: None,
        })
    }
}

#[derive(Debug)]
pub struct Vreg<RC: RegisterClass> {
    pub ty: TyIdx,
    pub class: RC,
}

#[derive(Default, Debug)]
pub struct VregInfo<RC: RegisterClass> {
    vreg_info: IndexVec<VregIdx, Vreg<RC>>,
}

impl<RC: RegisterClass> VregInfo<RC> {
    fn new() -> Self {
        Self {
            vreg_info: IndexVec::default(),
        }
    }

    pub fn create_vreg(&mut self, ty: TyIdx, class: RC) -> VregIdx {
        self.vreg_info.push(Vreg { ty, class })
    }

    pub fn get_vreg(&self, idx: VregIdx) -> &Vreg<RC> {
        &self.vreg_info[idx]
    }
}

#[derive(Default, Debug)]
pub struct StackObject {
    pub size: usize,
}

#[derive(Default, Debug)]
pub struct FrameInfo {
    objects: IndexVec<FrameIdx, StackObject>,
}

impl FrameInfo {
    pub fn create_stack_object(&mut self, size: usize) -> FrameIdx {
        self.objects.push(StackObject { size: size })
    }

    pub fn get_stack_object(&mut self, idx: FrameIdx) -> &StackObject {
        &self.objects[idx]
    }

    pub fn objects_iter(&self) -> impl Iterator<Item = &StackObject> {
        self.objects.iter()
    }
}
