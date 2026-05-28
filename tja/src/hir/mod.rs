mod basic_block;
mod function;
mod instruction;
mod lower;
mod module;
mod ty;

use basic_block::Block;
pub use basic_block::{BlockId, Builder as BlockBuilder};
use function::Function;
pub use function::{Builder as FunctionBuilder, Signature};
use instruction::{Instruction, InstructionId, Terminator};
pub(crate) use lower::lower;
pub use module::Module;
pub use ty::{Storage as TyStorage, Ty, TyIdx};

use crate::{FunctionIdx, GlobalVariableIdx, Immediate};
use derive_more::From;
use module::Declarations;
use std::{collections::BTreeMap, fmt::Display};

#[derive(From)]
pub enum Constant {
    GlobalVariable(GlobalVariableIdx),
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
    GlobalVariable(GlobalVariableIdx),
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
            Constant::GlobalVariable(var) => self
                .stack
                .push(ScalarIterNode::Scalar(ScalarConst::GlobalVariable(*var))),
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
            Constant::GlobalVariable(idx) => write!(f, "{}", self.decls.global_var(*idx).name),
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

pub enum GlobalVariable {
    Zero,
    Const(Constant),
}

pub struct DisplayGlobalVariable<'a> {
    module: &'a Module,
    ty_storage: &'a TyStorage,
    var: GlobalVariableIdx,
}

impl Display for DisplayGlobalVariable<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decl = self.module.decls.global_var(self.var);
        let global = self.module.global_vars.get(&self.var);

        write!(f, "{} = ", decl.name)?;

        if global.is_none() {
            write!(f, "external ")?;
        }

        write!(f, "global {}", decl.ty.display(self.ty_storage))?;

        if let Some(var) = global {
            write!(f, ", ")?;

            match var {
                GlobalVariable::Zero => write!(f, "zero")?,
                GlobalVariable::Const(const_) => {
                    write!(f, "{}", const_.display(&self.module.decls))?
                }
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
