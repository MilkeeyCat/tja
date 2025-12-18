use std::{io, process::Command};

fn generate_target_info(target: &'static str) -> io::Result<()> {
    let mut child = Command::new("python3")
        .env("PYTHONPATH", "../tools/dgen")
        .arg("-m")
        .arg("main")
        .arg("-t")
        .arg(target)
        .arg("-o")
        .arg(std::env::var("OUT_DIR").unwrap())
        .spawn()?;

    assert!(child.wait()?.success());

    Ok(())
}

fn main() -> io::Result<()> {
    generate_target_info("amd64")?;

    println!("cargo::rerun-if-changed=tools/dgen");

    Ok(())
}
