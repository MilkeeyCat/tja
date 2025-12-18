use crate::{
    hir::{Function, Module},
    pass::{Context, PassManager},
    targets::Target,
};

pub type ModulePassManager<'a, 'pass, T> = PassManager<'a, 'pass, Module, T>;
pub type FunctionPassManager<'a, 'pass, T> = PassManager<'a, 'pass, Function, T>;

impl<'a, 'pass, T: Target> ModulePassManager<'a, 'pass, T> {
    pub fn run(&self, module: &mut Module, ctx: &mut Context<'a, T>) {
        for pass in &self.passes {
            pass.run(module, ctx);
        }
    }
}

impl<'a, 'pass, T: Target> FunctionPassManager<'a, 'pass, T> {
    pub fn run(&self, function: &mut Function, ctx: &mut Context<'a, T>) {
        for pass in &self.passes {
            pass.run(function, ctx);
        }
    }
}
