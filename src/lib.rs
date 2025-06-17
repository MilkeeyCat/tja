#[macro_use]
extern crate impl_ops;

pub mod codegen;
pub mod hir;
pub mod lowering;
pub mod mir;
pub mod targets;

use codegen::generate;
use mir::Module;
use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};
use targets::Target;

pub struct CompileArgs {
    /// Produce assembly output
    pub assembly_only: bool,
    /// Compile and assemble but do not link
    pub object_only: bool,
    pub shared: bool,
}

pub fn compile(
    module: &mut Module,
    target: &dyn Target,
    args: CompileArgs,
) -> Result<(), Box<dyn std::error::Error>> {
    let code = generate(module, target)?;
    let name = module.name;

    if args.assembly_only {
        let asm_filename = PathBuf::from(&name).with_extension("s");
        let mut file = File::create(&asm_filename)?;

        file.write_all(&code)?;

        return Ok(());
    }

    let obj_filename = PathBuf::from(&name).with_extension("o");

    assemble(&code, &obj_filename)?;

    if args.object_only {
        return Ok(());
    }

    link(&obj_filename, Path::new(&name), args.shared)?;
    std::fs::remove_file(&obj_filename)?;

    Ok(())
}

fn assemble(source: &[u8], output: &Path) -> std::io::Result<()> {
    let source = Command::new("echo")
        .stdout(Stdio::piped())
        .arg(std::str::from_utf8(source).unwrap())
        .spawn()?;

    let args = vec![
        "-msyntax=intel",
        "-mnaked-reg",
        "-o",
        output.to_str().unwrap(),
    ];

    Command::new("as")
        .args(args)
        .stdin(Stdio::from(source.stdout.unwrap()))
        .spawn()?
        .wait()?;

    Ok(())
}

fn link(input: &Path, output: &Path, shared: bool) -> std::io::Result<()> {
    const OBJ_PATH: &'static str = "/usr/lib/x86_64-linux-gnu";
    let linker = format!("{OBJ_PATH}/ld-linux-x86-64.so.2");
    let crt = format!("{OBJ_PATH}/crt1.o");
    let mut args = vec![
        "-dynamic-linker",
        &linker,
        &crt,
        "-lc",
        input.to_str().unwrap(),
        "-z",
        "noexecstack",
        "-o",
        output.to_str().unwrap(),
    ];

    if shared {
        args.push("-shared");
    }

    Command::new("ld").args(args).spawn()?.wait()?;

    Ok(())
}
