use error_chain::error_chain;
use std::env;
use std::path::Path;
use std::process::Command;


error_chain!{
    errors { CmdError }
    foreign_links {
        Io(std::io::Error);
        Utf8(std::string::FromUtf8Error);
    }
}

fn main() {
    let output = compile_file().unwrap();
    println!("{}", output);
}

fn compile_file() -> Result<String> {
    let ghc_command = Command::new("ghc")
        .args(["-o", "test", "test.hs"])
        .output()
        .expect("failed to run ghc");
    
    if !ghc_command.status.success() {
        let err = String::from_utf8(ghc_command.stderr)?;
        error_chain::bail!("Failed to run ghc:\n {}", err)
    }
    
    let current_dir = env::current_dir()?;
    let path = Path::new(&current_dir).join("test.exe")
        .into_os_string()
        .into_string()
        .unwrap();

    let run_command = Command::new(path)
        .output()
        .expect("failed to execute compiled program");
    
    if !run_command.status.success() {
        let err = String::from_utf8(run_command.stderr)?;
        error_chain::bail!("Failed to run compiled program:\n {}", err)
    }
    
    let output = run_command.stdout;
    let raw_output = String::from_utf8(output)?;
    
    Ok(raw_output)
}

