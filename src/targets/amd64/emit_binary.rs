use crate::{
    mir::Module,
    targets::{Target, amd64::AsmPrinter},
    ty,
};
use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

pub struct Options {
    /// Produce assembly output
    pub assembly_only: bool,
    /// Compile and assemble but do not link
    pub object_only: bool,
    /// Link into a shared library
    pub shared: bool,
}

pub fn emit_binary<T: Target>(
    module: &mut Module,
    target: &T,
    ty_storage: &ty::Storage,
    options: Options,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut buf = String::new();
    let printer = AsmPrinter::new(target, ty_storage, &mut buf);
    printer.emit(module)?;

    let name = &module.name;

    if options.assembly_only {
        let asm_filename = PathBuf::from(&name).with_extension("s");
        let mut file = File::create(&asm_filename)?;

        file.write_all(buf.as_bytes())?;

        return Ok(());
    }

    let obj_filename = PathBuf::from(&name).with_extension("o");

    assemble(buf.as_bytes(), &obj_filename)?;

    if options.object_only {
        return Ok(());
    }

    link(&obj_filename, Path::new(&name), options.shared)?;
    std::fs::remove_file(&obj_filename)?;

    Ok(())
}

pub fn link(input: &Path, output: &Path, shared: bool) -> std::io::Result<()> {
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

    Command::new("ld").args(args).spawn()?.wait().map(|_| ())
}

pub fn assemble(source: &[u8], output: &Path) -> std::io::Result<()> {
    let args = vec![
        "-msyntax=intel",
        "-mnaked-reg",
        "-o",
        output.to_str().unwrap(),
    ];

    let mut command = Command::new("as")
        .args(args)
        .stdin(Stdio::piped())
        .spawn()?;

    command.stdin.as_mut().unwrap().write(source).map(|_| ())
}
