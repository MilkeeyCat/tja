use crate::{
    FunctionIdx, GlobalVariableIdx,
    lir::{GlobalVariable, module::Declarations},
    mir::{Function, Instruction},
};
use std::collections::HashMap;

pub(crate) struct Module<I: Instruction> {
    decls: Declarations,
    global_vars: HashMap<GlobalVariableIdx, GlobalVariable>,
    funcs: HashMap<FunctionIdx, Function<I>>,
}
