use crate::{hir::ty, mir, targets::Target};

pub struct Context<'hir, T: Target> {
    pub mir_module: Option<mir::Module>,
    pub mir_function: Option<mir::Function>,

    pub ty_storage: &'hir ty::Storage,
    pub target: T,
}

impl<'hir, T: Target> Context<'hir, T> {
    pub fn new(ty_storage: &'hir ty::Storage, target: T) -> Self {
        Self {
            mir_module: None,
            mir_function: None,

            ty_storage,
            target,
        }
    }
}

pub trait Pass<'hir, U, T: Target> {
    fn run(&self, unit: &mut U, ctx: &mut Context<'hir, T>);
}

pub struct PassManager<'hir, U, T: Target> {
    pub passes: Vec<Box<dyn Pass<'hir, U, T>>>,
}

impl<'hir, U, T: Target> PassManager<'hir, U, T> {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass<P: Pass<'hir, U, T> + Default + 'static>(&mut self) {
        self.passes.push(Box::new(P::default()));
    }
}
