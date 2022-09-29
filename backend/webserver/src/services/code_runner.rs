use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::process::Command;
use error_chain::error_chain;

const TEMP_DIRECTORY_NAME : &str = "haskell-code";
const TEMP_FILE_NAME : &str = "test";
const CODE_FILE_EXTENSION : &str = ".hs";

error_chain!{
    errors { CmdError }
    foreign_links {
        Io(std::io::Error);
        Utf8(std::string::FromUtf8Error);
    }
}

/// Executes the Haskell code in the string `code` and returns stdout.
pub fn execute(code : String) -> Result<String> {
    let executable_path = Path::new(TEMP_DIRECTORY_NAME)
        .join(TEMP_FILE_NAME)
        .into_os_string()
        .into_string()
        .unwrap(); // example: "haskell-code/test"

    let code_file_path = format!("{}{}", executable_path, CODE_FILE_EXTENSION); // example: "haskell-code/test.hs"
    
    write_code_to_file(&code, &code_file_path).expect("Could not write to file!");

    return compile_file(&code_file_path, &executable_path)
}

/// Writes given code to a file at path `code_file_path`.
fn write_code_to_file(code : &str, code_file_path: &str) -> std::io::Result<()> { 
    let mut file = File::create(code_file_path)?;
    file.write_all(code.as_bytes())?;

    drop(file);

    Ok(())
}

/// Compiles the given file at `code_file_path`, and outputs the executable at `executable_path`.
fn compile_file(code_file_path: &str, executable_path : &str) -> Result<String> {
    let ghc_command = Command::new("ghc")
        .args(["-o", executable_path, code_file_path])
        .output()
        .expect("failed to run ghc");
    
    if !ghc_command.status.success() {
        let err = String::from_utf8(ghc_command.stderr)?;
        error_chain::bail!("Failed to run ghc:\n {}", err)
    }
    
    let run_command = Command::new(executable_path)
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