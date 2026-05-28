use crate::{
    hir::{self, ScalarConst, TyStorage},
    lir::{self, ModuleBuilder},
    mir::{Abi, Target},
};

pub(crate) fn lower(
    hir_module: hir::Module,
    ty_storage: &TyStorage,
    target: &dyn Target,
) -> lir::Module {
    let decls = lower_decls(&hir_module.decls);
    let mut lir_module = lir::Module::new(decls);
    let mut builder = ModuleBuilder::new(&mut lir_module);

    lower_global_vars(&hir_module, &mut builder, ty_storage, target.abi());

    lir_module
}

fn lower_decls(hir_decls: &hir::module::Declarations) -> lir::module::Declarations {
    let global_vars = hir_decls
        .global_vars
        .iter()
        .map(|decl| lir::GlobalVariableDeclaration {
            name: decl.name.clone(),
        })
        .collect();

    lir::module::Declarations { global_vars }
}

fn lower_global_vars(
    module: &hir::Module,
    builder: &mut ModuleBuilder,
    ty_storage: &TyStorage,
    abi: &dyn Abi,
) {
    for (&idx, var) in &module.global_vars {
        let ty = module.decls.global_vars[idx].ty;
        let var = match var {
            hir::GlobalVariable::Zero => {
                let ty_size = abi.ty_size(ty_storage, ty);

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
