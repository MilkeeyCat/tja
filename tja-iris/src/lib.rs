mod ast;
mod codegen;
mod late;
mod lexer;
#[cfg(test)]
mod parser_tests;
mod sema;

use lalrpop_util::lalrpop_mod;
use tja_macros::define_index_type;

use crate::lexer::Lexer;
use std::path::Path;

lalrpop_mod!(grammar);

define_index_type! {
    pub(crate) struct TermDeclIdx = usize;
}

pub fn compile<P: AsRef<Path>>(input: &[P]) -> Result<String, std::fmt::Error> {
    let mut definitions = Vec::new();

    for path in input {
        let input = std::fs::read_to_string(path).expect("failed to read input file content");
        let lexer = Lexer::new(&input);

        definitions.extend(
            grammar::DefinitionsParser::new()
                .parse(lexer)
                .expect("failed to parse input"),
        );
    }

    let program = sema::run(definitions);
    let program = late::run(program);
    let mut buf = String::new();

    codegen::rust::generate(&program, &mut buf)?;

    Ok(buf)
}
