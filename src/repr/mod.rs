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
            Self::I8(_) | Self::U8(_) => Ty::I8,
            Self::I16(_) | Self::U16(_) => Ty::I16,
            Self::I32(_) | Self::U32(_) => Ty::I32,
            Self::I64(_) | Self::U64(_) => Ty::I64,
        }
    }

    pub fn usize_unchecked(&self) -> usize {
        match self {
            Const::I8(value) => (*value).try_into().unwrap(),
            Const::U8(value) => (*value).try_into().unwrap(),
            Const::I16(value) => (*value).try_into().unwrap(),
            Const::U16(value) => (*value).try_into().unwrap(),
            Const::I32(value) => (*value).try_into().unwrap(),
            Const::U32(value) => (*value).try_into().unwrap(),
            Const::I64(value) => (*value).try_into().unwrap(),
            Const::U64(value) => (*value).try_into().unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueTree {
    Leaf(Const),
    Branch(Vec<Self>),
}

impl ValueTree {
    pub fn ty(&self) -> Ty {
        match self {
            Self::Leaf(c) => c.ty(),
            Self::Branch(trees) => Ty::Struct(trees.iter().map(|tree| tree.ty()).collect()),
        }
    }
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
    Sext {
        operand: Operand,
        out: RegisterId,
    },
    Zext {
        operand: Operand,
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
    GetElementPtr {
        ty: Ty,
        base: Place,
        indices: Vec<Operand>,
        out: RegisterId,
    },
}

impl Instruction {
    pub fn def(&self) -> Option<RegisterId> {
        match self {
            Self::Binary { out, .. } => Some(*out),
            Self::Copy { out, .. } => Some(*out),
            Self::Sext { out, .. } => Some(*out),
            Self::Zext { out, .. } => Some(*out),
            Self::Alloca { out, .. } => Some(*out),
            Self::Store { .. } => None,
            Self::Load { out, .. } => Some(*out),
            Self::GetElementPtr { out, .. } => Some(*out),
        }
    }

    pub fn uses(&self) -> HashSet<RegisterId> {
        match self {
            Self::Binary { lhs, rhs, .. } => vec![lhs.register_id(), rhs.register_id()],
            Self::Copy { operand, .. } => vec![operand.register_id()],
            Self::Sext { operand, .. } => vec![operand.register_id()],
            Self::Zext { operand, .. } => vec![operand.register_id()],
            Self::Alloca { .. } => Vec::new(),
            Self::Store { place, value } => vec![place.register_id(), value.register_id()],
            Self::Load { place, .. } => vec![place.register_id()],
            Self::GetElementPtr { indices, base, .. } => {
                let mut uses: Vec<_> = indices
                    .iter()
                    .map(|operand| operand.register_id())
                    .collect();

                uses.push(base.register_id());

                uses
            }
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
