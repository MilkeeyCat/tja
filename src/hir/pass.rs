use crate::{
    hir::{Function, Module},
    pass::{Context, PassManager},
    targets::Target,
};

pub type ModulePassManager<'hir, T> = PassManager<'hir, Module, T>;
pub type FunctionPassManager<'hir, T> = PassManager<'hir, Function, T>;

impl<'hir, T: Target> ModulePassManager<'hir, T> {
    pub fn run(&self, module: &mut Module, ctx: &mut Context<'hir, T>) {
        for pass in &self.passes {
            pass.run(module, ctx);
        }
    }
}

impl<'hir, T: Target> FunctionPassManager<'hir, T> {
    pub fn run(&self, function: &mut Function, ctx: &mut Context<'hir, T>) {
        for pass in &self.passes {
            pass.run(function, ctx);
        }
    }
}
