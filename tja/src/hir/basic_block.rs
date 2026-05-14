use crate::hir::{
    Constant, Function, Instruction, InstructionId, Terminator, TyIdx, TyStorage, Value,
    module::Declarations,
};
use slotmap::new_key_type;

new_key_type! {
    pub struct BlockId;
}

pub struct Block {
    params: Vec<Value>,
    first_instr: Option<InstructionId>,
    last_instr: Option<InstructionId>,
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

    pub fn const_(&mut self, const_: Constant, ty: TyIdx) -> Value {
        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Const { const_ }, ty);

        self.func.instr_results(instr)[0]
    }

    pub fn ret(mut self, values: Vec<Value>) {
        let terminator = Terminator::Return(values.into());

        self.inserter.insert_terminator(self.func, terminator);
    }
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
