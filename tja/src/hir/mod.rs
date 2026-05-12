mod function;
pub mod module;
mod ty;

pub use function::{Function, FunctionIdx, Signature};
pub use module::{Constant, Global, GlobalIdx, Immediate, Module};
pub use ty::{Storage as TyStorage, Ty};
