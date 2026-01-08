mod ast;
mod lower;
mod visitor;

use lalrpop_util::lalrpop_mod;
use std::path::Path;

lalrpop_mod!(pub grammar);

pub fn compile(input: &Path) {
    let input = std::fs::read_to_string(input).expect("failed to read input file content");
    let definitions = grammar::DefinitionsParser::new()
        .parse(&input)
        .expect("failed to parse input");
    let module = lower::run(definitions);
}
