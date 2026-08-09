use crate::{
    FunctionIdx, GlobalVariableIdx,
    lir::{GlobalVariable, module::Declarations},
    mir::{Function, Target},
};
use std::collections::HashMap;

pub(super) struct Module<T: Target> {
    decls: Declarations,
    global_vars: HashMap<GlobalVariableIdx, GlobalVariable>,
    funcs: HashMap<FunctionIdx, Function<T>>,
}
