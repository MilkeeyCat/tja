mod ast;
mod lexer;
#[cfg(test)]
mod parser_tests;
mod sema;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);
