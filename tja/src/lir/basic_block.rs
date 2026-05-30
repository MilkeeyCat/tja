use crate::{
    Immediate,
    lir::{
        Function, GlobalValueIdx, Instruction, InstructionId, Terminator, Ty, Value,
        module::Declarations,
    },
};
use slotmap::new_key_type;

new_key_type! {
    pub(super) struct BlockId;
}

pub(super) struct Block {
    pub(super) params: Vec<Value>,
    pub(super) first_instr: Option<InstructionId>,
    pub(super) last_instr: Option<InstructionId>,
    terminator: Option<Terminator>,
}

impl Block {
    pub(super) fn new(block: BlockId, params: Vec<Ty>) -> Self {
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

    pub(super) fn set_terminator(&mut self, terminator: Terminator) {
        self.terminator = Some(terminator);
    }
}

trait InstructionInserter {
    fn insert_instr(
        &mut self,
        func: &mut Function,
        instr: Instruction,
        ty: Option<Ty>,
    ) -> InstructionId;
    fn insert_terminator(&mut self, func: &mut Function, terminator: Terminator);
}

#[allow(private_bounds)]
pub(crate) struct Builder<'a, I: InstructionInserter> {
    inserter: I,
    func: &'a mut Function,
    decls: &'a Declarations,
}

#[allow(private_bounds)]
impl<'a, I: InstructionInserter> Builder<'a, I> {
    pub(super) fn new(inserter: I, func: &'a mut Function, decls: &'a Declarations) -> Self {
        Self {
            inserter,
            func,
            decls,
        }
    }

    pub(crate) fn iconst(&mut self, imm: Immediate, ty: Ty) -> Value {
        assert!(ty.is_int());
        assert!(imm_fits_in_ty(imm, ty));

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Iconst { imm }, Some(ty));

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn load(&mut self, ptr: Value, ty: Ty) -> Value {
        assert_eq!(ptr.ty(), Ty::PTR);

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Load { ptr }, Some(ty));

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn store(&mut self, ptr: Value, value: Value) -> Value {
        assert_eq!(ptr.ty(), Ty::PTR);

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Store { ptr, value }, None);

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn shl(&mut self, value: Value, bits: Value) -> Value {
        assert!(value.ty().is_int());
        assert_eq!(bits.ty(), Ty::I8);

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Shl { value, bits }, None);

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn lshr(&mut self, value: Value, bits: Value) -> Value {
        assert!(value.ty().is_int());
        assert_eq!(bits.ty(), Ty::I8);

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Lshr { value, bits }, None);

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn global_value_ptr(&mut self, value: GlobalValueIdx) -> Value {
        let instr =
            self.inserter
                .insert_instr(self.func, Instruction::GlobalValuePtr { value }, None);

        self.func.instr_results(instr)[0]
    }
}

fn imm_fits_in_ty(imm: Immediate, ty: Ty) -> bool {
    match ty {
        Ty::I8 => ((i8::MIN as i64)..=(i8::MAX as i64)).contains(&imm.0),
        Ty::I16 => ((i16::MIN as i64)..=(i16::MAX as i64)).contains(&imm.0),
        Ty::I32 => ((i32::MIN as i64)..=(i32::MAX as i64)).contains(&imm.0),
        Ty::I64 => true,
        _ => unreachable!(),
    }
}

pub(super) struct AppendInstrInserter<'a> {
    decls: &'a Declarations,
    block: BlockId,
}

impl<'a> AppendInstrInserter<'a> {
    pub(super) fn new(decls: &'a Declarations, block: BlockId) -> Self {
        Self { decls, block }
    }
}

impl InstructionInserter for AppendInstrInserter<'_> {
    fn insert_instr(
        &mut self,
        func: &mut Function,
        instr: Instruction,
        ty: Option<Ty>,
    ) -> InstructionId {
        let results = match &instr {
            Instruction::Iconst { .. } => vec![ty.unwrap()],
            Instruction::Load { .. } => vec![ty.unwrap()],
            Instruction::Store { .. } => vec![],
            Instruction::Shl { value, .. } => vec![value.ty()],
            Instruction::Lshr { value, .. } => vec![value.ty()],
            Instruction::GlobalValuePtr { .. } => vec![Ty::PTR],
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
