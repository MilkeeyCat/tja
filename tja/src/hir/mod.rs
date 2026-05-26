mod basic_block;
mod function;
mod instruction;
mod module;
mod ty;

use basic_block::Block;
pub use basic_block::{BlockId, Builder as BlockBuilder};
use function::Function;
pub use function::{Builder as FunctionBuilder, FunctionIdx, Signature};
use instruction::{Instruction, InstructionId, Terminator};
pub use module::Module;
pub use ty::{Storage as TyStorage, Ty, TyIdx};

use derive_more::From;
use index_vec::define_index_type;
use module::Declarations;
use std::{collections::BTreeMap, fmt::Display};

#[derive(From, Clone, Copy)]
#[from(u8, i8, u16, i16, u32, i32, i64)]
pub struct Immediate(i64);

#[derive(From)]
pub enum Constant {
    Global(GlobalIdx),
    Function(FunctionIdx),
    Imm(Immediate),
    Aggregate(Vec<Self>),
}

impl Constant {
    fn display<'a>(&'a self, decls: &'a Declarations) -> DisplayConstant<'a> {
        DisplayConstant {
            decls,
            const_: self,
        }
    }

    pub(crate) fn scalar_iter<'a>(&'a self) -> ScalarIter<'a> {
        ScalarIter::new(self)
    }
}

pub(crate) enum ScalarConst {
    Global(GlobalIdx),
    Function(FunctionIdx),
    Imm(Immediate),
}

enum ScalarIterNode<'a> {
    Aggregate(&'a [Constant]),
    Scalar(ScalarConst),
}

pub(crate) struct ScalarIter<'a> {
    stack: Vec<ScalarIterNode<'a>>,
}

impl<'a> ScalarIter<'a> {
    fn new(const_: &'a Constant) -> Self {
        let mut iter = Self { stack: Vec::new() };

        iter.push(const_);

        iter
    }

    fn push(&mut self, const_: &'a Constant) {
        match const_ {
            Constant::Global(global) => self
                .stack
                .push(ScalarIterNode::Scalar(ScalarConst::Global(*global))),
            Constant::Function(func) => self
                .stack
                .push(ScalarIterNode::Scalar(ScalarConst::Function(*func))),
            Constant::Imm(imm) => self
                .stack
                .push(ScalarIterNode::Scalar(ScalarConst::Imm(*imm))),
            Constant::Aggregate(values) => self.stack.push(ScalarIterNode::Aggregate(values)),
        }
    }
}

impl Iterator for ScalarIter<'_> {
    type Item = ScalarConst;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node) = self.stack.pop() {
            match node {
                ScalarIterNode::Aggregate(values) => match values {
                    [const_, rest @ ..] => {
                        if !rest.is_empty() {
                            self.stack.push(ScalarIterNode::Aggregate(rest));
                        }

                        self.push(const_);
                    }
                    [] => continue,
                },
                ScalarIterNode::Scalar(scalar) => return Some(scalar),
            }
        }

        None
    }
}

pub struct DisplayConstant<'a> {
    decls: &'a Declarations,
    const_: &'a Constant,
}

impl Display for DisplayConstant<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.const_ {
            Constant::Global(idx) => write!(f, "{}", self.decls.global(*idx).name),
            Constant::Function(idx) => write!(f, "{}", self.decls.function(*idx).name),
            Constant::Imm(imm) => write!(f, "{}", imm.0),
            Constant::Aggregate(consts) => {
                write!(f, "{{")?;

                let mut iter = consts.iter().peekable();

                while let Some(const_) = iter.next() {
                    write!(f, "{}", const_.display(self.decls))?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "}}")?;

                Ok(())
            }
        }
    }
}

define_index_type! {
    pub struct GlobalIdx = usize;
}

pub enum Global {
    Zero,
    Const(Constant),
}

pub struct DisplayGlobal<'a> {
    module: &'a Module,
    ty_storage: &'a TyStorage,
    global: GlobalIdx,
}

impl Display for DisplayGlobal<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decl = self.module.decls.global(self.global);
        let global = self.module.globals.get(&self.global);

        write!(f, "{} = ", decl.name)?;

        if global.is_none() {
            write!(f, "external ")?;
        }

        write!(f, "global {}", decl.ty.display(self.ty_storage))?;

        if let Some(global) = global {
            write!(f, ", ")?;

            match global {
                Global::Zero => write!(f, "zero")?,
                Global::Const(const_) => write!(f, "{}", const_.display(&self.module.decls))?,
            };
        }

        Ok(())
    }
}

#[allow(private_interfaces)]
#[derive(Debug, Clone, Copy)]
pub enum Value {
    Param {
        ty: TyIdx,
        block: BlockId,
        idx: usize,
    },
    Instr {
        ty: TyIdx,
        instr: InstructionId,
        result_idx: usize,
    },
}

impl Value {
    pub fn ty(&self) -> TyIdx {
        match self {
            Self::Param { ty, .. } => *ty,
            Self::Instr { ty, .. } => *ty,
        }
    }

    fn display<'a>(&'a self, instr_to_idx: &'a BTreeMap<InstructionId, usize>) -> DisplayValue<'a> {
        DisplayValue {
            instr_to_idx,
            value: self,
        }
    }
}

pub struct DisplayValue<'a> {
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    value: &'a Value,
}

impl Display for DisplayValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Param { idx, .. } => write!(f, "p{}", idx),
            Value::Instr {
                instr, result_idx, ..
            } => write!(f, "v{}_{}", self.instr_to_idx[instr], result_idx),
        }
    }
}
