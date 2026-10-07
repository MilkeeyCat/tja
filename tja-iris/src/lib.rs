mod ast;
mod lexer;
#[cfg(test)]
mod parser_tests;
mod sema;

use lalrpop_util::lalrpop_mod;
use tja_macros::define_index_type;

lalrpop_mod!(grammar);

define_index_type! {
    pub(crate) struct TermDeclIdx = usize;
}
