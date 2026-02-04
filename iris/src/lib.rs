mod ast;
mod decision;
mod generator;
mod lexer;
mod lower;
mod visitor;

use lalrpop_util::lalrpop_mod;
use lexer::Lexer;
use std::path::Path;

lalrpop_mod!(pub grammar);

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

    let module = lower::run(definitions);

    generator::generate(module)
}
