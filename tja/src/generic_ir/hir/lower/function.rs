use crate::{
    FunctionIdx,
    hir::{self, Function, TyStorage, constant::ScalarConst},
    lir::{self, FunctionBuilder, ParamRanges},
    mir::Abi,
};
use smallvec::SmallVec;
use std::collections::HashMap;

pub(crate) struct LoweringCtx<'a, TI: hir::TargetInstruction> {
    pub(crate) lir_func_builder: FunctionBuilder<'a, TI::LirTargetInstr>,
    pub(crate) ty_storage: &'a TyStorage,
    pub(crate) abi: &'a dyn Abi<TargetInstruction = TI>,
    pub(crate) hir_func: &'a Function<TI>,

    param_ranges: &'a HashMap<FunctionIdx, ParamRanges>,
    values: HashMap<hir::Value, SmallVec<[lir::Value; 1]>>,
}

impl<TI: hir::TargetInstruction> LoweringCtx<'_, TI> {
    pub(crate) fn lower_value(&mut self, value: hir::Value, values: Vec<lir::Value>) {
        assert!(self.values.insert(value, values.into()).is_none());
    }

    pub(crate) fn lower_param(&mut self, value: hir::Value, values: Vec<lir::Value>) {
        assert!(matches!(value, hir::Value::Param { .. }));

        self.lower_value(value, values);
    }

    pub(crate) fn lowered_value(&self, value: hir::Value) -> &[lir::Value] {
        &self.values[&value]
    }

    fn lower_block(&mut self, block: hir::BlockId) {
        let func_idx = self.hir_func.idx;
        let is_entry_block = self.lir_func_builder.func.entry_block().is_none();
        let params = if is_entry_block {
            self.lir_func_builder.decls.funcs[func_idx]
                .sig
                .params
                .iter()
                .map(|value| value.ty)
                .collect()
        } else {
            self.hir_func
                .block_params(block)
                .iter()
                .map(|value| value.ty().lir_ty_iter(self.ty_storage))
                .flatten()
                .collect()
        };
        let lir_block = self.lir_func_builder.create_block(params);

        self.lir_func_builder.select_block(lir_block);

        if is_entry_block {
            self.abi.calling_conv().lower_entry_block_params(
                self,
                self.hir_func.block_params(block),
                &self.lir_func_builder.decls.funcs[func_idx]
                    .sig
                    .params
                    .clone()
                    .into_iter()
                    .zip(self.lir_func_builder.func.block_params(lir_block).to_vec())
                    .collect::<Vec<_>>(),
                &self.param_ranges[&func_idx],
            );
        }
    }

    fn lower_instr(&mut self, instr: hir::InstructionId) {
        match self.hir_func.instr(instr) {
            hir::Instruction::Const { const_ } => {
                let value = self.hir_func.instr_results(instr)[0];
                let mut builder = self.lir_func_builder.block_builder();
                let values = const_
                    .scalar_iter()
                    .zip(value.ty().lir_ty_iter(self.ty_storage))
                    .map(|(scalar, ty)| match scalar {
                        ScalarConst::GlobalVariable(var) => {
                            builder.global_value_ptr(lir::GlobalValueIdx::GlobalValue(var))
                        }
                        ScalarConst::Function(func) => {
                            builder.global_value_ptr(lir::GlobalValueIdx::Function(func))
                        }
                        ScalarConst::Imm(imm) => builder.iconst(imm, ty),
                    })
                    .collect();

                self.lower_value(value, values);
            }
            hir::Instruction::Target(target_instr) => target_instr.lower(self, instr),
        }
    }

    fn lower_terminator(&mut self, block: hir::BlockId) {
        match self.hir_func.terminator(block) {
            hir::Terminator::Return(value) => {
                self.abi.calling_conv().lower_ret(
                    self,
                    value.map(|value| {
                        (
                            value,
                            self.lir_func_builder.decls.funcs[self.hir_func.idx]
                                .sig
                                .returns
                                .as_slice(),
                        )
                    }),
                );
            }
        }
    }
}

pub(super) fn lower<TI: hir::TargetInstruction>(
    func: &Function<TI>,
    ty_storage: &TyStorage,
    param_ranges: &HashMap<FunctionIdx, ParamRanges>,
    abi: &dyn Abi<TargetInstruction = TI>,
    builder: FunctionBuilder<TI::LirTargetInstr>,
) {
    let mut ctx = LoweringCtx {
        hir_func: func,
        ty_storage,
        param_ranges,
        abi,
        lir_func_builder: builder,
        values: HashMap::new(),
    };

    for block in func.blocks_iter() {
        ctx.lower_block(block);

        for instr in func.instrs_iter(block) {
            ctx.lower_instr(instr);
        }

        ctx.lower_terminator(block);
    }
}
