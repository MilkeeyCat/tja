pub mod codegen;
pub mod repr;

use codegen::CodeGen;
use repr::{Module, Wrapper};
use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

pub struct CompileArgs {
    /// Output binary file name
    pub output: Option<PathBuf>,
    /// Produce assembly output
    pub assembly_only: bool,
    /// Compile and assemble but do not link
    pub object_only: bool,
    pub shared: bool,
}

pub fn compile(module: Wrapper<'_, &mut Module>, args: CompileArgs) -> std::io::Result<()> {
    let code = CodeGen::new(module).compile();

    if args.assembly_only {
        let asm_filename = args
            .output
            .unwrap_or_else(|| "main".into())
            .with_extension("s");
        let mut file = File::create(&asm_filename)?;

        file.write_all(&code)?;

        return Ok(());
    }

    let obj_filename = args
        .output
        .clone()
        .unwrap_or_else(|| "main".into())
        .with_extension("o");

    assemble(&code, &obj_filename)?;

    if args.object_only {
        return Ok(());
    }

    let binary_filename = args.output.unwrap_or_else(|| "a.out".into());

    link(&obj_filename, &binary_filename, args.shared)?;
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
