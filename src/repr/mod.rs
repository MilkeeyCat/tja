mod function;
pub mod op;
pub mod ty;

pub use function::Function;
use op::BinOp;
use std::collections::HashSet;
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
            Self::Place(place) => place.register_id(),
            Self::Const(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Place {
    Register(RegisterId),
    Global(GlobalId),
}

impl Place {
    pub fn register_id(&self) -> Option<RegisterId> {
        match self {
            Self::Register(r) => Some(*r),
            Self::Global(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug)]
pub enum Instruction {
    Binary {
        kind: BinOp,
        ty: Ty,
        lhs: Operand,
        rhs: Operand,
        place: Place,
    },
    Copy {
        place: Place,
        operand: Operand,
    },
}

impl Instruction {
    pub fn def(&self) -> Option<RegisterId> {
        match self {
            Self::Binary {
                place: Place::Register(r),
                ..
            } => Some(*r),
            Self::Copy {
                place: Place::Register(r),
                ..
            } => Some(*r),
            _ => None,
        }
    }

    pub fn uses(&self) -> HashSet<RegisterId> {
        match self {
            Self::Binary { lhs, rhs, .. } => vec![lhs.register_id(), rhs.register_id()],
            Self::Copy { operand, .. } => vec![operand.register_id()],
        }
        .into_iter()
        .flatten()
        .collect()
    }
}

#[derive(Debug)]
pub enum Terminator {
    Goto(BlockId),
    Return(Option<Operand>),
}

impl Terminator {
    pub fn uses(&self) -> HashSet<RegisterId> {
        match self {
            Self::Return(Some(Operand::Place(Place::Register(r)))) => HashSet::from([*r]),
            Self::Return(_) | Self::Goto(_) => HashSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct Global {
    pub name: String,
    pub ty: Ty,
    pub value: Const,
}

#[derive(Debug)]
pub struct Register {
    pub name: String,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct Program {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
    //TODO: add aggregate types
}
