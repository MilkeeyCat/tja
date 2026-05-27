use crate::hir::{
    Constant, Function, Instruction, InstructionId, Terminator, Ty, TyIdx, TyStorage, Value,
    module::Declarations,
};
use slotmap::new_key_type;

new_key_type! {
    pub struct BlockId;
}

pub(super) struct Block {
    pub(super) params: Vec<Value>,
    pub(super) first_instr: Option<InstructionId>,
    pub(super) last_instr: Option<InstructionId>,
    terminator: Option<Terminator>,
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

    pub(super) fn terminator(&self) -> &Terminator {
        self.terminator.as_ref().unwrap()
    }

    pub(super) fn set_terminator(&mut self, terminator: Terminator) {
        self.terminator = Some(terminator);
    }
}

pub(super) trait InstructionInserter {
    fn insert_instr(&mut self, func: &mut Function, instr: Instruction, ty: TyIdx)
    -> InstructionId;
    fn insert_terminator(&mut self, func: &mut Function, terminator: Terminator);
}

#[allow(private_bounds)]
pub struct Builder<'a, I: InstructionInserter> {
    inserter: I,
    func: &'a mut Function,
    decls: &'a Declarations,
    ty_storage: &'a TyStorage,
}

#[allow(private_bounds)]
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

    pub fn const_(&mut self, const_: Constant, ty: TyIdx) -> Value {
        if let Err(err) = validate_const(&const_, ty, self.ty_storage, vec![]) {
            panic!("{}", err.display(&const_, ty, self.decls, self.ty_storage));
        }

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Const { const_ }, ty);

        self.func.instr_results(instr)[0]
    }

    pub fn ret(mut self, value: Option<Value>) {
        let sig = &self.decls.function(self.func.idx).sig;

        assert_eq!(
            sig.return_,
            value.map(|value| value.ty()),
            "signatures are not equal"
        );

        let terminator = Terminator::Return(value);

        self.inserter.insert_terminator(self.func, terminator);
    }
}

enum ValidateConstErrorKind {
    UnexpectedValue { expected_ty: TyIdx },
    SizeMismatch { expected: usize, actual: usize },
}

struct ValidateConstError {
    projection: Vec<usize>,
    kind: ValidateConstErrorKind,
}

impl ValidateConstError {
    fn display(
        self,
        const_: &Constant,
        ty: TyIdx,
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
        };

        return format!(
            "const {} doesn't match {} type, projection {} - {}",
            const_.display(decls),
            ty.display(ty_storage),
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
}

fn validate_const(
    const_: &Constant,
    ty: TyIdx,
    ty_storage: &TyStorage,
    projection: Vec<usize>,
) -> Result<(), ValidateConstError> {
    match ty_storage.get(ty) {
        Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 => {
            if !matches!(const_, Constant::Imm(..)) {
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
        func.block_mut(self.block).set_terminator(terminator);
    }
}

pub(super) struct BlocksIter<'a> {
    func: &'a Function,
    next: Option<BlockId>,
}

impl<'a> BlocksIter<'a> {
    pub(super) fn new(func: &'a Function, start: Option<BlockId>) -> Self {
        Self { func, next: start }
    }
}

impl Iterator for BlocksIter<'_> {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        let block = self.next?;

        self.next = self.func.next_block(block);

        Some(block)
    }
}
