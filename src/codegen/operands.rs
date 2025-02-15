use super::register::Register;
use crate::repr::Const;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum OperandSize {
    Byte,
    Word,
    Dword,
    Qword,
}

impl std::fmt::Display for OperandSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Byte => "byte ptr",
                Self::Word => "word ptr",
                Self::Dword => "dword ptr",
                Self::Qword => "qword ptr",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Default, Copy)]
pub struct Offset(pub isize);

impl std::ops::Add<isize> for &Offset {
    type Output = Offset;

    fn add(self, rhs: isize) -> Self::Output {
        Offset(self.0 + rhs)
    }
}

impl std::ops::Sub<isize> for &Offset {
    type Output = Offset;

    fn sub(self, rhs: isize) -> Self::Output {
        Offset(self.0 - rhs)
    }
}

impl std::ops::Add<&Offset> for &Offset {
    type Output = Offset;

    fn add(self, rhs: &Offset) -> Self::Output {
        Offset(self.0 + rhs.0)
    }
}

impl std::ops::Sub<&Offset> for &Offset {
    type Output = Offset;

    fn sub(self, rhs: &Offset) -> Self::Output {
        Offset(self.0 - rhs.0)
    }
}

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

#[derive(Clone, Debug, PartialEq)]
pub enum Base {
    Register(Register),
    Label(String),
}

impl std::fmt::Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(r) => r.fmt(f),
            Self::Label(label) => label.fmt(f),
        }
    }
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

#[derive(Debug, Clone)]
pub enum Source {
    Memory(Memory),
    Register(Register),
    Immediate(Const),
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

impl std::fmt::Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8(num) => num.fmt(f),
            Self::U8(num) => num.fmt(f),
        }
    }
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Memory(mem) => mem.fmt(f),
            Self::Register(r) => r.fmt(f),
            Self::Immediate(imm) => imm.fmt(f),
        }
    }
}

impl Into<Source> for Const {
    fn into(self) -> Source {
        Source::Immediate(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
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
}

impl Into<Source> for Destination {
    fn into(self) -> Source {
        match self {
            Self::Memory(mem) => Source::Memory(mem),
            Self::Register(r) => Source::Register(r),
        }
    }
}

impl Into<EffectiveAddress> for Destination {
    fn into(self) -> EffectiveAddress {
        match self {
            Self::Memory(mem) => mem.effective_address,
            Self::Register(r) => EffectiveAddress {
                base: Base::Register(r),
                index: None,
                scale: None,
                displacement: None,
            },
        }
    }
}

impl std::fmt::Display for Destination {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Memory(mem) => mem.fmt(f),
            Self::Register(r) => r.fmt(f),
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
