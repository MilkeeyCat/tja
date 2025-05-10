use super::register::Register;
use derive_more::{Display, From};
use impl_ops::{impl_op_ex, impl_op_ex_commutative};
use std::ops;

#[derive(Copy, Clone, Debug, Display, PartialEq, PartialOrd)]
pub enum OperandSize {
    #[display("byte ptr")]
    Byte = 1,
    #[display("word ptr")]
    Word = 2,
    #[display("dword ptr")]
    Dword = 4,
    #[display("qword ptr")]
    Qword = 8,
}

#[derive(Debug)]
pub struct InvalidOperandSize;

impl TryFrom<usize> for OperandSize {
    type Error = InvalidOperandSize;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(match value {
            1 => Self::Byte,
            2 => Self::Word,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => return Err(InvalidOperandSize),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Default, Copy)]
pub struct Offset(pub isize);

impl_op_ex!(+ |lhs: &Offset, rhs: &Offset| -> Offset {Offset(lhs.0 + rhs.0)});
impl_op_ex!(-|lhs: &Offset, rhs: &Offset| -> Offset { Offset(lhs.0 - rhs.0) });
impl_op_ex_commutative!(+ |lhs: &Offset, rhs: &isize| -> Offset {Offset(lhs.0 + *rhs)});
impl_op_ex!(-|lhs: &Offset, rhs: &isize| -> Offset { Offset(lhs.0 - *rhs) });

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 > 0 {
            write!(f, " + {}", self.0)
        } else if self.0 < 0 {
            write!(f, " - {}", self.0.abs())
        } else {
            write!(f, "")
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum Base {
    Register(Register),
    Label(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct EffectiveAddress {
    pub base: Base,
    pub index: Option<Register>,
    pub scale: Option<usize>,
    pub displacement: Option<Offset>,
}

impl EffectiveAddress {
    pub fn src(&self, size: OperandSize) -> Source {
        Source::Memory(Memory {
            effective_address: self.clone(),
            size,
        })
    }

    pub fn dest(&self, size: OperandSize) -> Destination {
        Destination::Memory(Memory {
            effective_address: self.clone(),
            size,
        })
    }
}

impl From<Destination> for EffectiveAddress {
    fn from(value: Destination) -> Self {
        match value {
            Destination::Memory(mem) => mem.effective_address,
            Destination::Register(r) => Self {
                base: Base::Register(r),
                index: None,
                scale: None,
                displacement: None,
            },
        }
    }
}

impl From<Register> for EffectiveAddress {
    fn from(value: Register) -> Self {
        EffectiveAddress {
            base: Base::Register(value),
            displacement: None,
            index: None,
            scale: None,
        }
    }
}

impl std::fmt::Display for EffectiveAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = format!("[{}", self.base);

        if let Some(index) = &self.index {
            str.push_str(&format!(" + {}", index));
        }

        if let Some(scale) = self.scale {
            str.push_str(&format!("* {scale}"));
        }

        if let Some(displacement) = &self.displacement {
            str.push_str(&format!("{displacement}"));
        }

        write!(f, "{}]", str)
    }
}

impl std::ops::Add<Offset> for EffectiveAddress {
    type Output = EffectiveAddress;

    fn add(mut self, rhs: Offset) -> Self::Output {
        self.displacement = Some(&self.displacement.unwrap_or_default() + &rhs);

        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Memory {
    pub effective_address: EffectiveAddress,
    pub size: OperandSize,
}

impl std::fmt::Display for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.size, self.effective_address)
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum Immediate {
    Int(u64),
    Label(String),
}

#[derive(Debug, Clone, Display, PartialEq, From)]
pub enum Source {
    Memory(Memory),
    Register(Register),
    Immediate(Immediate),
}

impl Source {
    pub fn size(&self) -> Option<OperandSize> {
        match self {
            Self::Memory(mem) => Some(mem.size),
            Self::Register(r) => Some(r.size()),
            Self::Immediate(_) => None,
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
pub enum Destination {
    Memory(Memory),
    Register(Register),
}

impl Destination {
    pub fn size(&self) -> OperandSize {
        match self {
            Self::Memory(mem) => mem.size.clone(),
            Self::Register(reg) => reg.size(),
        }
    }

    pub fn resize(mut self, size: OperandSize) -> Self {
        match &mut self {
            Self::Memory(mem) => {
                mem.size = size;

                self
            }
            Self::Register(r) => r.resize(size).into(),
        }
    }
}

impl From<Register> for Destination {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

impl From<Destination> for Source {
    fn from(value: Destination) -> Self {
        match value {
            Destination::Memory(mem) => Self::Memory(mem),
            Destination::Register(r) => Self::Register(r),
        }
    }
}

impl PartialEq<&Source> for &Destination {
    fn eq(&self, other: &&Source) -> bool {
        match (self, other) {
            (Destination::Memory(lhs), Source::Memory(rhs)) if lhs == rhs => true,
            (Destination::Register(lhs), Source::Register(rhs)) if lhs == rhs => true,
            _ => false,
        }
    }
}

impl PartialEq<&Destination> for &Source {
    fn eq(&self, other: &&Destination) -> bool {
        other == self
    }
}
