use crate::{
    FunctionIdx, GlobalVariableIdx, Immediate,
    hir::{Declarations, Ty, TyIdx, TyStorage},
};
use derive_more::From;
use std::fmt::Display;

#[derive(From)]
pub enum Constant {
    GlobalVariable(GlobalVariableIdx),
    Function(FunctionIdx),
    Imm(Immediate),
    Aggregate(Vec<Self>),
}

impl Constant {
    pub(super) fn validate(
        &self,
        ty: TyIdx,
        ty_storage: &TyStorage,
    ) -> Result<(), ValidateConstError> {
        validate(self, ty, ty_storage, Vec::new())
    }

    pub(super) fn display<'a>(&'a self, decls: &'a Declarations) -> DisplayConstant<'a> {
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

enum ValidateConstErrorKind {
    UnexpectedValue { expected_ty: TyIdx },
    SizeMismatch { expected: usize, actual: usize },
    ImmediateOutOfRange { imm: Immediate, ty: TyIdx },
}

pub(super) struct ValidateConstError {
    projection: Vec<usize>,
    kind: ValidateConstErrorKind,
}

impl ValidateConstError {
    pub(super) fn display(
        self,
        const_: &Constant,
        decls: &Declarations,
        ty_storage: &TyStorage,
    ) -> String {
        let err = match self.kind {
            ValidateConstErrorKind::SizeMismatch { expected, actual } => {
                format!("size mismatch, expected {}, got {}", expected, actual)
            }
            ValidateConstErrorKind::UnexpectedValue { expected_ty } => {
                format!(
                    "unexpected type, expected {}",
                    expected_ty.display(ty_storage)
                )
            }
            ValidateConstErrorKind::ImmediateOutOfRange { imm, ty } => {
                format!(
                    "immediate {} out of range for {}",
                    imm,
                    ty.display(ty_storage)
                )
            }
        };

        return format!(
            "const {}, projection {} - {}",
            const_.display(decls),
            format!(
                "[{}]",
                self.projection
                    .into_iter()
                    .map(|idx| idx.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            err
        );
    }
}

impl ValidateConstError {
    fn unexpected_value(projection: Vec<usize>, expected_ty: TyIdx) -> Self {
        Self {
            projection,
            kind: ValidateConstErrorKind::UnexpectedValue { expected_ty },
        }
    }

    fn size_mismatch(projection: Vec<usize>, expected: usize, actual: usize) -> Self {
        Self {
            projection,
            kind: ValidateConstErrorKind::SizeMismatch { expected, actual },
        }
    }

    fn imm_out_of_range(projection: Vec<usize>, imm: Immediate, ty: TyIdx) -> Self {
        Self {
            projection,
            kind: ValidateConstErrorKind::ImmediateOutOfRange { imm, ty },
        }
    }
}

fn validate(
    const_: &Constant,
    ty: TyIdx,
    ty_storage: &TyStorage,
    projection: Vec<usize>,
) -> Result<(), ValidateConstError> {
    match ty_storage.get(ty) {
        int_ty @ (Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64) => {
            if let &Constant::Imm(imm) = const_ {
                if !imm_fits_in_ty(imm, int_ty) {
                    return Err(ValidateConstError::imm_out_of_range(projection, imm, ty));
                }
            } else {
                return Err(ValidateConstError::unexpected_value(projection, ty));
            }
        }
        Ty::Ptr => {
            if !matches!(
                const_,
                Constant::GlobalVariable(..) | Constant::Function(..)
            ) {
                return Err(ValidateConstError::unexpected_value(projection, ty));
            }
        }
        Ty::Struct(tys) => {
            let Constant::Aggregate(consts) = const_ else {
                return Err(ValidateConstError::unexpected_value(projection, ty));
            };

            if tys.len() != consts.len() {
                return Err(ValidateConstError::size_mismatch(
                    projection,
                    tys.len(),
                    consts.len(),
                ));
            }

            for ((idx, &ty), const_) in tys.iter().enumerate().zip(consts) {
                validate(
                    const_,
                    ty,
                    ty_storage,
                    projection
                        .clone()
                        .into_iter()
                        .chain(std::iter::once(idx))
                        .collect(),
                )?;
            }
        }
        &Ty::Array { ty: elem_ty, len } => {
            let Constant::Aggregate(consts) = const_ else {
                return Err(ValidateConstError::unexpected_value(projection, ty));
            };

            if len != consts.len() {
                return Err(ValidateConstError::size_mismatch(
                    projection,
                    len,
                    consts.len(),
                ));
            }

            for (idx, const_) in (0..len).into_iter().zip(consts) {
                validate(
                    const_,
                    elem_ty,
                    ty_storage,
                    projection
                        .clone()
                        .into_iter()
                        .chain(std::iter::once(idx))
                        .collect(),
                )?;
            }
        }
    }

    Ok(())
}

fn imm_fits_in_ty(imm: Immediate, ty: &Ty) -> bool {
    match ty {
        Ty::I8 => ((i8::MIN as i64)..=(i8::MAX as i64)).contains(&imm.0),
        Ty::I16 => ((i16::MIN as i64)..=(i16::MAX as i64)).contains(&imm.0),
        Ty::I32 => ((i32::MIN as i64)..=(i32::MAX as i64)).contains(&imm.0),
        Ty::I64 => true,
        _ => unreachable!(),
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
