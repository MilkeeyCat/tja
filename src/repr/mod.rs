mod op;

use op::BinOp;

pub type LocalId = usize;
pub type GlobalId = usize;
pub type BlockId = usize;

pub enum Place {
    Local(LocalId),
    Global(GlobalId),
}

pub enum Operand {
    Place(Place),
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
}

pub struct BasicBlock {
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

pub struct Function {
    blocks: Vec<BasicBlock>,
    //TODO: add locals
}

pub struct Program {
    functions: Vec<Function>,
    //TODO: add aggregate types
}
