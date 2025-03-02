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
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
}

impl Const {
    pub fn ty(&self) -> Ty {
        match self {
            Self::I8(_) => Ty::I8,
            Self::U8(_) => Ty::U8,
            Self::I16(_) => Ty::I16,
            Self::U16(_) => Ty::U16,
            Self::I32(_) => Ty::I32,
            Self::U32(_) => Ty::U32,
            Self::I64(_) => Ty::I64,
            Self::U64(_) => Ty::U64,
        }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        lhs: Operand,
        rhs: Operand,
        out: RegisterId,
    },
    Copy {
        operand: Operand,
        out: RegisterId,
    },
    Alloca {
        ty: Ty,
        out: RegisterId,
    },
    Store {
        place: Place,
        value: Operand,
    },
    Load {
        place: Place,
        out: RegisterId,
    },
}

impl Instruction {
    pub fn def(&self) -> Option<RegisterId> {
        match self {
            Self::Binary { out, .. } => Some(*out),
            Self::Copy { out, .. } => Some(*out),
            Self::Alloca { out, .. } => Some(*out),
            Self::Store { .. } => None,
            Self::Load { out, .. } => Some(*out),
        }
    }

    pub fn uses(&self) -> HashSet<RegisterId> {
        match self {
            Self::Binary { lhs, rhs, .. } => vec![lhs.register_id(), rhs.register_id()],
            Self::Copy { operand, .. } => vec![operand.register_id()],
            Self::Alloca { .. } => Vec::new(),
            Self::Store { place, value } => vec![place.register_id(), value.register_id()],
            Self::Load { place, .. } => vec![place.register_id()],
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
