use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::process::{Command, Stdio};
use error_chain::error_chain;
use std::time::Duration;
use wait_timeout::ChildExt;

const TIME_OUT : u64 = 10;
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
    compile_file(&code_file_path, &executable_path).expect("Could not compile file!");

    return run_file(&executable_path)
}

/// Writes given code to a file at path `code_file_path`.
fn write_code_to_file(code : &str, code_file_path: &str) -> std::io::Result<()> { 
    let mut file = File::create(code_file_path)?;
    file.write_all(code.as_bytes())?;

    drop(file);

    Ok(())
}

/// Compiles the given file at `code_file_path`, and outputs the executable at `executable_path`.
fn compile_file(code_file_path: &str, executable_path : &str) -> Result<()> {
    let ghc_command = Command::new("ghc")
        .args(["-O0", "-o", executable_path, code_file_path])
        .output()
        .expect("failed to run ghc");

    if !ghc_command.status.success() {
        let mut err = String::from_utf8(ghc_command.stderr)?;
        err = format_haskell_stdout(&err);
        error_chain::bail!(err)
    }
    
    Ok(())
}

fn run_file(executable_path : &str) -> Result<String> {
    let mut child = Command::new(executable_path)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let secs = Duration::from_secs(TIME_OUT);

    return match child.wait_timeout(secs).unwrap() {
        Some(_status) => {
            let mut s = String::new();
            child.stdout.unwrap().read_to_string(&mut s).unwrap();

            Ok(s)
        },
        None => {
            child.kill().unwrap();
            child.wait().unwrap();
            error_chain::bail!("Code execution timed out.")
        }
    };
}

fn format_haskell_stdout(output : &str) -> String {
    let mut split_output : Vec<&str> = output.split("\r\n").collect();
    split_output[0] = "";
    split_output[1] = "An error occurred:\r\n";

    return split_output.iter().map(|s| s.to_string()).collect()
}