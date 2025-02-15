mod basic_block;
mod function;
pub mod op;
pub mod ty;

pub use basic_block::BasicBlock;
pub use function::Function;
use op::BinOp;
use ty::Ty;

pub type RegisterId = usize;
pub type GlobalId = usize;
pub type BlockId = usize;

#[derive(Debug, Clone)]
pub enum Const {
    I8(i8),
    U8(u8),
}

#[derive(Debug)]
pub enum ValueTree {
    Leaf(Const),
    Branch(Vec<Self>),
}

#[derive(Debug)]
pub enum Operand {
    Place(Place),
    Const(ValueTree),
}

impl Operand {
    pub fn register_id(&self) -> Option<RegisterId> {
        match self {
            Self::Place(place) => match place {
                Place::Register(r) => Some(*r),
                Place::Global(_) => None,
            },
            Self::Const(_) => None,
        }
    }
}

#[derive(Debug)]
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
    Copy {
        place: Place,
        operand: Operand,
    },
}

pub enum Terminator {
    Goto(BlockId),
    Return(Option<Operand>),
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

pub struct Program {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
    //TODO: add aggregate types
}
