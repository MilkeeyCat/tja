use std::{io, process::Command};

fn generate_target_info(target: &'static str) -> io::Result<()> {
    let mut child = Command::new("python3")
        .env("PYTHONPATH", "tools")
        .arg("-m")
        .arg("dgen.main")
        .arg("-t")
        .arg(target)
        .spawn()?;

    assert!(child.wait()?.success());

    Ok(())
}

fn main() -> io::Result<()> {
    generate_target_info("amd64")?;

    println!("cargo::rerun-if-changed=tools/dgen");

    Ok(())
}
