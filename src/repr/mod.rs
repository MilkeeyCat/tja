pub mod op;
pub mod ty;

use op::BinOp;
use ty::Ty;

pub type RegisterId = usize;
pub type GlobalId = usize;
pub type BlockId = usize;

pub enum Const {
    I8(i8),
    U8(u8),
}

pub enum ValueTree {
    Leaf(Const),
    Branch(Vec<Self>),
}

pub enum Operand {
    Place(Place),
    Const(ValueTree),
}

pub enum Place {
    Register(RegisterId),
    Global(GlobalId),
}

pub enum Instruction {
    Binary {
        kind: BinOp,
        lhs: Operand,
        rhs: Operand,
        place: Place,
    },
}

pub enum Terminator {
    Goto(BlockId),
    Return(Option<Operand>),
}

pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

pub struct Global {
    pub name: String,
    pub ty: Ty,
    pub value: Const,
}

pub struct Register {
    pub name: String,
    pub ty: Ty,
}

pub struct Function {
    pub name: String,
    pub params: Vec<RegisterId>,
    pub ret_ty: Ty,
    pub blocks: Vec<BasicBlock>,
    pub registers: Vec<Register>,
}

pub struct Program {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
    //TODO: add aggregate types
}
