use crate::{
    hir::{self, TyStorage},
    lir,
};

pub enum Instruction {
    Rdtsc,
}

impl super::InstrName for Instruction {
    fn name(&self) -> &'static str {
        match self {
            Self::Rdtsc => "rdtsc",
        }
    }
}

#[allow(private_interfaces)]
impl hir::TargetInstruction for Instruction {
    type LirTargetInstr = Self;

    fn fmt(&self, _ctx: &hir::instruction::DisplayInstr<Self>, _f: &mut std::fmt::Formatter<'_>) {
        match self {
            Self::Rdtsc => (),
        }
    }

    fn result_tys(&self, ty_storage: &mut TyStorage, _ty: Option<hir::TyIdx>) -> Vec<hir::TyIdx> {
        match self {
            Self::Rdtsc => {
                vec![ty_storage.add(hir::Ty::Struct(vec![ty_storage.i32_ty; 2]))]
            }
        }
    }

    fn lower(&self, ctx: &mut hir::FuncLoweringCtx<'_, Self>, instr: hir::InstructionId) {
        match self {
            Self::Rdtsc => {
                let value = ctx.hir_func.instr_results(instr)[0];
                let results = ctx.lir_func_builder.block_builder().rdtsc();

                ctx.lower_value(value, results.to_vec())
            }
        }
    }
}

impl lir::TargetInstruction for Instruction {
    fn fmt(&self, _ctx: &lir::instruction::DisplayInstr<Self>, _f: &mut std::fmt::Formatter<'_>) {
        match self {
            Self::Rdtsc => (),
        }
    }

    fn result_tys(&self, _ty: Option<lir::Ty>) -> Vec<lir::Ty> {
        match self {
            Self::Rdtsc => {
                vec![lir::Ty::I32; 2]
            }
        }
    }
}

#[allow(private_bounds)]
impl<I: hir::basic_block::InstructionInserter<Instruction>> hir::BlockBuilder<'_, Instruction, I> {
    pub fn rdtsc(&mut self) -> hir::Value {
        let instr = self.inserter.insert_instr(
            self.func,
            self.ty_storage,
            hir::Instruction::Target(Instruction::Rdtsc),
            None,
        );

        self.func.instr_results(instr)[0]
    }
}

impl<I: lir::basic_block::InstructionInserter<Instruction>> lir::BlockBuilder<'_, Instruction, I> {
    pub fn rdtsc(&mut self) -> [lir::Value; 2] {
        let instr = self.inserter.insert_instr(
            self.func,
            lir::Instruction::Target(Instruction::Rdtsc),
            None,
        );

        self.func.instr_results(instr).try_into().unwrap()
    }
}
