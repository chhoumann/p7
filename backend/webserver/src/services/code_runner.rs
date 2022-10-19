use std::fs::File;
use std::io::{prelude::*, Stdout};
use std::path::Path;
use std::process::{Command, Stdio, Child, ExitStatus, ChildStdin, ChildStdout, ChildStderr};
use std::thread::Thread;
use error_chain::{error_chain, ExitCode};
use std::time::Duration;
use wait_timeout::ChildExt;

use super::dir_generator;

const TIME_OUT : u64 = 10;
const TEMP_CODE_FILE_NAME : &str = "code.hs";
const TEMP_TEST_FILE_NAME : &str = "test.hs";

const TEST_CODE : &str = r#"
import Test.Hspec
import Tet.QuickCheck
import Code (add)
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "should evaluate 2 + 2 = 4" $ do
      add 2 2 `shouldBe` (4 :: Int)
"#;

error_chain!{
    errors { CmdError }
    foreign_links {
        Io(std::io::Error);
        Utf8(std::string::FromUtf8Error);
    }
}


/// Executes the Haskell code in the string `code` and returns stdout.
pub fn execute(code : String, test : String) -> Result<String> {
    // Generate temp directory and code files containing the code and associated test
    let dir = dir_generator::generate_dir();

    generate_file(&dir, TEMP_CODE_FILE_NAME, &code);
    generate_file(&dir, TEMP_TEST_FILE_NAME, TEST_CODE);

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
        println!("runhaskell process encountered stderr.");
    }
    else {
        runhaskell_process.stdout.unwrap().read_to_string(&mut output).unwrap();
    }

    return output
}

fn format_haskell_stdout(output : String) -> String {
    let mut split_output : Vec<&str> = output.split("\r\n").collect();

    if split_output.len() < 2 {
        return output
    }

    split_output[0] = "";
    split_output[1] = "An error occurred:\r\n";

    return split_output.iter().map(|s| s.to_string()).collect()
}

fn clean_up_code_dir(dir : &str) {
    std::fs::remove_dir_all(dir).unwrap();
}