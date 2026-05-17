use crate::hir::{
    Constant, Function, Instruction, InstructionId, Terminator, Ty, TyIdx, TyStorage, Value,
    module::Declarations,
};
use slotmap::new_key_type;

new_key_type! {
    pub struct BlockId;
}

pub struct Block {
    params: Vec<Value>,
    pub(super) first_instr: Option<InstructionId>,
    pub(super) last_instr: Option<InstructionId>,
    pub(super) terminator: Option<Terminator>,
}

impl Block {
    pub(super) fn new(block: BlockId, params: Vec<TyIdx>) -> Self {
        Self {
            params: params
                .into_iter()
                .enumerate()
                .map(|(idx, ty)| Value::Param { ty, block, idx })
                .collect(),
            first_instr: None,
            last_instr: None,
            terminator: None,
        }
    }
}

pub trait InstructionInserter {
    fn insert_instr(&mut self, func: &mut Function, instr: Instruction, ty: TyIdx)
    -> InstructionId;
    fn insert_terminator(&mut self, func: &mut Function, terminator: Terminator);
}

pub struct Builder<'a, I: InstructionInserter> {
    inserter: I,
    func: &'a mut Function,
    decls: &'a Declarations,
    ty_storage: &'a TyStorage,
}

impl<'a, I: InstructionInserter> Builder<'a, I> {
    pub(super) fn new(
        inserter: I,
        func: &'a mut Function,
        decls: &'a Declarations,
        ty_storage: &'a TyStorage,
    ) -> Self {
        Self {
            inserter,
            func,
            decls,
            ty_storage,
        }
    }

    pub fn const_(&mut self, const_: Constant, ty: TyIdx) -> Result<Value, ConstValidationError> {
        validate_const(&const_, ty, self.ty_storage, vec![])?;

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Const { const_ }, ty);

        Ok(self.func.instr_results(instr)[0])
    }

    pub fn ret(mut self, values: Vec<Value>) -> Result<(), SignatureMismatchError> {
        let sig = &self.decls.get_function(self.func.idx()).sig;
        let tys = values.iter().map(|value| value.ty()).collect();

        if sig.returns != tys {
            return Err(SignatureMismatchError {
                expected: sig.returns.clone(),
                actual: tys,
            });
        }

        let terminator = Terminator::Return(values.into());

        self.inserter.insert_terminator(self.func, terminator);

        Ok(())
    }
}

#[derive(Debug)]
pub struct SignatureMismatchError {
    pub expected: Vec<TyIdx>,
    pub actual: Vec<TyIdx>,
}

#[derive(Debug)]
pub enum ConstValidationErrorKind {
    UnexpectedValue { expected_ty: TyIdx },
    SizeMismatch { expected: usize, actual: usize },
}

#[derive(Debug)]
pub struct ConstValidationError {
    pub projection: Vec<usize>,
    pub kind: ConstValidationErrorKind,
}

impl ConstValidationError {
    fn unexpected_value(projection: Vec<usize>, expected_ty: TyIdx) -> Self {
        Self {
            projection,
            kind: ConstValidationErrorKind::UnexpectedValue { expected_ty },
        }
    }

    fn size_mismatch(projection: Vec<usize>, expected: usize, actual: usize) -> Self {
        Self {
            projection,
            kind: ConstValidationErrorKind::SizeMismatch { expected, actual },
        }
    }
}

fn validate_const(
    const_: &Constant,
    ty: TyIdx,
    ty_storage: &TyStorage,
    projection: Vec<usize>,
) -> Result<(), ConstValidationError> {
    match ty_storage.get(ty) {
        Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 => {
            if !matches!(const_, Constant::Imm(..)) {
                return Err(ConstValidationError::unexpected_value(projection, ty));
            }
        }
        Ty::Ptr => {
            if !matches!(const_, Constant::Global(..) | Constant::Function(..)) {
                return Err(ConstValidationError::unexpected_value(projection, ty));
            }
        }
        Ty::Struct(tys) => {
            let Constant::Aggregate(consts) = const_ else {
                return Err(ConstValidationError::unexpected_value(projection, ty));
            };

            if tys.len() != consts.len() {
                return Err(ConstValidationError::size_mismatch(
                    projection,
                    tys.len(),
                    consts.len(),
                ));
            }

            for ((idx, &ty), const_) in tys.iter().enumerate().zip(consts) {
                validate_const(
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
                return Err(ConstValidationError::unexpected_value(projection, ty));
            };

            if len != consts.len() {
                return Err(ConstValidationError::size_mismatch(
                    projection,
                    len,
                    consts.len(),
                ));
            }

            for (idx, const_) in (0..len).into_iter().zip(consts) {
                validate_const(
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

pub struct AppendInstrInserter<'a> {
    decls: &'a Declarations,
    ty_storage: &'a TyStorage,
    block: BlockId,
}

impl<'a> AppendInstrInserter<'a> {
    pub(super) fn new(decls: &'a Declarations, ty_storage: &'a TyStorage, block: BlockId) -> Self {
        Self {
            decls,
            ty_storage,
            block,
        }
    }
}

impl InstructionInserter for AppendInstrInserter<'_> {
    fn insert_instr(
        &mut self,
        func: &mut Function,
        instr: Instruction,
        ty: TyIdx,
    ) -> InstructionId {
        let results = match &instr {
            Instruction::Const { .. } => vec![ty],
        };
        let instr = func.create_instr(instr);

        func.append_instr(instr, self.block);

        if !results.is_empty() {
            func.create_instr_results(
                instr,
                results
                    .into_iter()
                    .enumerate()
                    .map(|(idx, ty)| Value::Instr {
                        ty,
                        instr,
                        result_idx: idx,
                    })
                    .collect(),
            );
        }

        instr
    }

    fn insert_terminator(&mut self, func: &mut Function, terminator: Terminator) {
        func.set_terminator(self.block, terminator);
    }
}
