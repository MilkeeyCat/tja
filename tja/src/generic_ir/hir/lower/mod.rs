mod function;

pub(crate) use function::LoweringCtx as FuncLoweringCtx;

use crate::{
    FunctionIdx,
    hir::{self, TyStorage, constant::ScalarConst},
    lir::{self, FunctionBuilder, ModuleBuilder, ParamRanges},
    mir::{Abi, Target},
};
use std::collections::HashMap;

pub(crate) fn lower<TI: hir::TargetInstruction>(
    hir_module: hir::Module<TI>,
    ty_storage: &TyStorage,
    target: &dyn Target<TargetInstruction = TI>,
) -> lir::Module<TI::LirTargetInstr> {
    let (decls, param_ranges) = lower_decls(&hir_module.decls, ty_storage, target.abi());
    let mut lir_module = lir::Module::new(decls);
    let mut builder = ModuleBuilder::new(&mut lir_module);

    lower_global_vars(&hir_module, &mut builder, ty_storage, target.abi());

    for (&idx, func) in &hir_module.funcs {
        builder.define_function(idx);
        function::lower(
            func,
            ty_storage,
            &param_ranges,
            target.abi(),
            FunctionBuilder::new(builder.0, idx),
        );
    }

    lir_module
}

fn lower_decls<TI: hir::TargetInstruction>(
    hir_decls: &hir::module::Declarations,
    ty_storage: &TyStorage,
    abi: &dyn Abi<TargetInstruction = TI>,
) -> (lir::module::Declarations, HashMap<FunctionIdx, ParamRanges>) {
    let (funcs, param_ranges) = hir_decls
        .funcs
        .iter_enumerated()
        .map(|(idx, decl)| {
            let (sig, ranges) = abi
                .calling_conv()
                .lower_signature(abi, ty_storage, &decl.sig);

            (
                lir::FunctionDeclaration {
                    name: decl.name.clone(),
                    sig,
                },
                (idx, ranges),
            )
        })
        .unzip();
    let global_vars = hir_decls
        .global_vars
        .iter()
        .map(|decl| lir::GlobalVariableDeclaration {
            name: decl.name.clone(),
        })
        .collect();

    (
        lir::module::Declarations { funcs, global_vars },
        param_ranges,
    )
}

fn lower_global_vars<TI: hir::TargetInstruction>(
    module: &hir::Module<TI>,
    builder: &mut ModuleBuilder<TI::LirTargetInstr>,
    ty_storage: &TyStorage,
    abi: &dyn Abi<TargetInstruction = TI>,
) {
    for (&idx, var) in &module.global_vars {
        let ty = module.decls.global_vars[idx].ty;
        let var = match var {
            hir::GlobalVariable::Zero => {
                let ty_size = abi.hir_ty_size(ty_storage, ty);

                lir::GlobalVariable::Zero(ty_size)
            }
            hir::GlobalVariable::Const(const_) => {
                let values = ty
                    .offset_iter(ty_storage, abi)
                    .zip(const_.scalar_iter())
                    .zip(ty.lir_ty_iter(ty_storage))
                    .map(|((offset, const_), ty)| {
                        let value = match const_ {
                            ScalarConst::GlobalVariable(idx) => {
                                lir::GlobalVariableValue::GlobalVariable(idx)
                            }
                            ScalarConst::Function(idx) => lir::GlobalVariableValue::Function(idx),
                            ScalarConst::Imm(imm) => lir::GlobalVariableValue::Imm(ty, imm),
                        };

                        (offset, value)
                    })
                    .collect();

                lir::GlobalVariable::Value(values)
            }
        };

        builder.define_global_var(idx, var);
    }
}
