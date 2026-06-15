use crate::{
    Immediate,
    lir::{
        Function, GlobalValueIdx, Instruction, InstructionId, Terminator, Ty, Value,
        module::Declarations,
    },
};
use slotmap::new_key_type;

new_key_type! {
    pub(crate) struct BlockId;
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

    pub(super) fn terminator(&self) -> &Terminator {
        self.terminator.as_ref().unwrap()
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

    pub(crate) fn iconst<Imm: Into<Immediate>>(&mut self, imm: Imm, ty: Ty) -> Value {
        let imm: Immediate = imm.into();

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

    pub(crate) fn store(&mut self, ptr: Value, value: Value) {
        assert_eq!(ptr.ty(), Ty::PTR);

        self.inserter
            .insert_instr(self.func, Instruction::Store { ptr, value }, None);
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

    pub(crate) fn zext(&mut self, value: Value, ty: Ty) -> Value {
        assert!(is_valid_cast(value.ty(), ty, false));

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Zext { value }, Some(ty));

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn trunc(&mut self, value: Value, ty: Ty) -> Value {
        assert!(is_valid_cast(value.ty(), ty, true));

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Trunc { value }, Some(ty));

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn ptr_add(&mut self, ptr: Value, offset: Value) -> Value {
        assert_eq!(ptr.ty(), Ty::PTR);
        assert!(offset.ty().is_int());

        let instr =
            self.inserter
                .insert_instr(self.func, Instruction::PtrAdd { ptr, offset }, None);

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn int_to_ptr(&mut self, int: Value) -> Value {
        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::IntToPtr { int }, None);

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn ptr_to_int(&mut self, ptr: Value, ty: Ty) -> Value {
        assert!(ptr.ty() == Ty::PTR);
        assert!(ty.is_int());

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::PtrToInt { ptr }, Some(ty));

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn or(&mut self, lhs: Value, rhs: Value) -> Value {
        assert!(lhs.ty().is_int());
        assert!(lhs.ty() == rhs.ty());

        let instr = self
            .inserter
            .insert_instr(self.func, Instruction::Or { lhs, rhs }, None);

        self.func.instr_results(instr)[0]
    }

    pub(crate) fn ret(&mut self, values: Vec<Value>) {
        let sig = &self.decls.funcs[self.func.idx].sig;

        assert!(
            sig.returns
                .iter()
                .map(|value| value.ty)
                .eq(values.iter().map(|value| value.ty())),
            "signatures are not equal"
        );

        let terminator = Terminator::Return(values.into());

        self.inserter.insert_terminator(self.func, terminator);
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

fn is_valid_cast(from: Ty, to: Ty, is_trunc: bool) -> bool {
    const CAST_ORDER: &'static [Ty] = &[Ty::I8, Ty::I16, Ty::I32, Ty::I64];
    let from = CAST_ORDER.iter().position(|ty| *ty == from);
    let to = CAST_ORDER.iter().position(|ty| *ty == to);

    match (from, to) {
        (Some(from), Some(to)) if from > to && is_trunc => true,
        (Some(from), Some(to)) if from < to && !is_trunc => true,
        _ => false,
    }
}

pub(crate) struct AppendInstrInserter<'a> {
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
            Instruction::Zext { .. } => vec![ty.unwrap()],
            Instruction::Trunc { .. } => vec![ty.unwrap()],
            Instruction::PtrAdd { .. } => vec![Ty::PTR],
            Instruction::IntToPtr { .. } => vec![Ty::PTR],
            Instruction::PtrToInt { .. } => vec![ty.unwrap()],
            Instruction::Or { lhs, .. } => vec![lhs.ty()],
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
