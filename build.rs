use std::{path::PathBuf, process::Command};

fn main() {
    if !PathBuf::from(".git").exists() {
        return;
    }

    // Re-run on git commit.
    if PathBuf::from(".git/HEAD").exists() {
        println!("cargo:rerun-if-changed=.git/HEAD");
        let head_ref = std::fs::read_to_string(".git/HEAD").unwrap();
        let mut head_ref = head_ref.split_ascii_whitespace();
        if let Some(branch) = head_ref.nth(1) {
            println!("cargo:rerun-if-changed=.git/{}", branch);
        }
    }

    let output = match Command::new("git")
        .arg("log")
        .arg("-1")
        .arg("--date=short")
        .arg("--format=%H %h %cd")
        .output()
    {
        Ok(output) if output.status.success() => output,
        _ => return,
    };
    let stdout = String::from_utf8(output.stdout).unwrap();
    let mut parts = stdout.split_whitespace();
    let mut next = || parts.next().unwrap();
    println!("cargo:rustc-env=GDN_COMMIT_HASH={}", next());
    println!("cargo:rustc-env=GDN_COMMIT_SHORT_HASH={}", next());
    println!("cargo:rustc-env=GDN_COMMIT_DATE={}", next())
}
