use crate::{hir, mir, targets::Target, ty};

pub struct Context<'a, T: Target> {
    pub mir_module: Option<mir::Module>,
    pub mir_function: Option<mir::Function>,

    pub ty_storage: &'a ty::Storage,
    pub target: &'a T,
}

impl<'a, T: Target> Context<'a, T> {
    pub fn new(ty_storage: &'a ty::Storage, target: &'a T) -> Self {
        Self {
            mir_module: None,
            mir_function: None,

            ty_storage,
            target,
        }
    }
}

pub trait Pass<'a, U, T: Target> {
    fn run(&self, unit: &mut U, ctx: &mut Context<'a, T>);
}

pub struct PassManager<'a, 'pass, U, T: Target> {
    pub passes: Vec<Box<dyn Pass<'a, U, T> + 'pass>>,
}

impl<'a, 'pass, U, T: Target> PassManager<'a, 'pass, U, T> {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass<P: Pass<'a, U, T> + 'pass>(&mut self, pass: P) {
        self.passes.push(Box::new(pass));
    }
}

pub struct FunctionToModuleAdapter<'a, 'pass, U, T: Target>(PassManager<'a, 'pass, U, T>);

impl<'a, 'pass, U, T: Target> FunctionToModuleAdapter<'a, 'pass, U, T> {
    pub fn new(pass_manager: PassManager<'a, 'pass, U, T>) -> Self {
        Self(pass_manager)
    }
}

impl<'a, 'pass, T: Target> Pass<'a, hir::Module, T>
    for FunctionToModuleAdapter<'a, 'pass, hir::Function, T>
{
    fn run(&self, module: &mut hir::Module, ctx: &mut Context<'a, T>) {
        for func in &mut module.functions {
            for pass in &self.0.passes {
                pass.run(func, ctx);
            }
        }
    }
}

impl<'a, 'pass, T: Target> Pass<'a, mir::Module, T>
    for FunctionToModuleAdapter<'a, 'pass, mir::Function, T>
{
    fn run(&self, module: &mut mir::Module, ctx: &mut Context<'a, T>) {
        for func in &mut module.functions {
            for pass in &self.0.passes {
                pass.run(func, ctx);
            }
        }
    }
}
