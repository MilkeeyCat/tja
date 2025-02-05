pub mod op;
pub mod ty;

use op::BinOp;
use ty::Ty;

pub type LocalId = usize;
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
    Local(LocalId),
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

pub struct Variable {
    pub name: String,
    pub ty: Ty,
}

pub struct Function {
    pub name: String,
    pub blocks: Vec<BasicBlock>,
    pub locals: Vec<Variable>,
    //TODO: add locals
}

pub struct Program {
    pub globals: Vec<Variable>,
    pub functions: Vec<Function>,
    //TODO: add aggregate types
}
