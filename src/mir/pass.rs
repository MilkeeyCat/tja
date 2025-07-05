use crate::{
    mir::{Function, Module},
    pass::{Context, PassManager},
    targets::Target,
};

pub type ModulePassManager<'a, 'hir, T> = PassManager<'hir, &'a Module<'hir>, T>;
pub type FunctionPassManager<'a, 'hir, T> = PassManager<'hir, &'a Function<'hir>, T>;

impl<'a, 'hir, T: Target> ModulePassManager<'a, 'hir, T> {
    pub fn run(&self, module: &'a Module<'hir>, ctx: &mut Context<'hir, T>) {
        for pass in &self.passes {
            pass.run(module, ctx);
        }
    }
}

impl<'a, 'hir, T: Target> FunctionPassManager<'a, 'hir, T> {
    pub fn run(&self, function: &'a Function<'hir>, ctx: &mut Context<'hir, T>) {
        for pass in &self.passes {
            pass.run(function, ctx);
        }
    }
}
