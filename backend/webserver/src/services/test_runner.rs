use std::fs::File;
use std::io::{prelude::*};
use std::path::Path;
use std::process::{Command, Stdio, Child, ChildStdin, ChildStdout, ChildStderr};
use error_chain::{error_chain};
use rocket::tokio;
use wait_timeout::ChildExt;
use tokio::time::Duration;

use super::dir_generator;

const TIME_OUT : u64 = 10;
const TEMP_CODE_FILE_NAME : &str = "Code.hs";
const TEMP_TEST_FILE_NAME : &str = "Test.hs";

error_chain!{
    errors { CmdError }
    foreign_links {
        Io(std::io::Error);
        Utf8(std::string::FromUtf8Error);
    }
}


/// Executes the Haskell code in the string `code` and returns stdout.
pub async fn execute(exercise_code: String, test_code: String) -> Result<String> {
    // Generate temp directory and code files containing the code and associated test
    let dir = dir_generator::generate_dir();

    generate_file(&dir, TEMP_CODE_FILE_NAME, &exercise_code);
    generate_file(&dir, TEMP_TEST_FILE_NAME, &test_code);

    println!("Running tests using runhaskell...");

    // Spawn "runhaskell" child process and kill after TIME_OUT
    let mut runhaskell_process = spawn_runhaskell_command(&dir, TEMP_TEST_FILE_NAME);

    let secs = Duration::from_secs(TIME_OUT);
    let status_code = match runhaskell_process.wait_timeout(secs).unwrap() {
        Some(status) => status,
        None => {
            runhaskell_process.kill().unwrap();
            error_chain::bail!(format!("Code execution timed out after {} seconds.", TIME_OUT))
        }
    };

    // Remove temporary directories and files, and result of the runhaskell command
    clean_up_code_dir(&dir);

    let output = get_output(runhaskell_process);

    if !status_code.success() {
        // Note: Tests that fail flush to stdout and not stderr
        println!("Test failed!\nOutput: {}", output);
        error_chain::bail!(output)
    }

    println!("Test succeeded!");

    return Ok(output)
}

fn generate_file(dir : &str, file_name : &str, content : &str) {
    let file_path = Path::new(dir)
        .join(file_name)
        .into_os_string()
        .into_string()
        .unwrap(); // example: "haskell-code/code"

    write_code_to_file(content, &file_path)
        .expect("Could not write to file!");
}

/// Writes given code to a file at path `code_file_path`.
fn write_code_to_file(code : &str, file_path: &str) -> std::io::Result<()> {
    println!("Writing code to file {}...", file_path);

    let mut file = File::create(file_path)?;
    file.write_all(code.as_bytes())?;

    drop(file);

    Ok(())
}

/// Spawns a child runhaskell process with `dir` as its working directory
fn spawn_runhaskell_command(dir : &str, file_name : &str) -> Child {
    return Command::new("runhaskell")
        .current_dir(dir)
        .arg(file_name)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
}

fn get_output(runhaskell_process : Child) -> String {
    let mut output = String::new();

    if !runhaskell_process.stderr.is_none() {
        runhaskell_process.stderr.unwrap().read_to_string(&mut output).unwrap();

        if !output.is_empty() {
            println!("runhaskell process encountered stderr: {}", output);
            return output
        }
    }

    runhaskell_process.stdout.unwrap().read_to_string(&mut output).unwrap();

    return output
}

fn clean_up_code_dir(dir : &str) {
    std::fs::remove_dir_all(dir).unwrap();
}