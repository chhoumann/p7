use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::process::{Command, Stdio};
use error_chain::error_chain;
use std::time::Duration;
use wait_timeout::ChildExt;

use super::dir_generator;

const TIME_OUT : u64 = 10;
const TEMP_CODE_FILE_NAME : &str = "code";
const TEMP_TEST_FILE_NAME : &str = "test";
const CODE_FILE_EXTENSION : &str = ".hs";

const TEST_CODE : &str = r#"
import Test.Hspec
import Test.QuickCheck
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
    let dir = dir_generator::generate_dir();

    generate_file(&dir, TEMP_CODE_FILE_NAME, &code);
    let test_path = generate_file(&dir, TEMP_TEST_FILE_NAME, TEST_CODE);

    let runhaskell_command = Command::new("runhaskell")
        .env("GHC_PACKAGE_PATH", dir)
        .arg(test_path)
        .output()
        .unwrap();
   
    if !runhaskell_command.status.success() {
        let err = String::from_utf8(runhaskell_command.stderr)?;
        error_chain::bail!(err)
    }
    
    let result = String::from_utf8(runhaskell_command.stdout).unwrap();

    // clean_up_code_dir(&dir);

    return Ok(result)
}

fn generate_file(dir : &str, file_name : &str, content : &str) -> String {
    let path = Path::new(dir)
        .join(file_name)
        .into_os_string()
        .into_string()
        .unwrap(); // example: "haskell-code/code"

    let file_path = format!("{}{}", path, CODE_FILE_EXTENSION); // example: "haskell-code/code.hs"
    
    write_code_to_file(content, &file_path)
        .expect("Could not write to file!");

    return file_path
}

/// Writes given code to a file at path `code_file_path`.
fn write_code_to_file(code : &str, code_file_path: &str) -> std::io::Result<()> { 
    println!("Writing code to file...");

    let mut file = File::create(code_file_path)?;
    file.write_all(code.as_bytes())?;

    drop(file);

    Ok(())
}

/// Compiles the given file at `code_file_path`, and outputs the executable at `executable_path`.
fn compile_file(code_file_path: &str, executable_path : &str) -> Result<()> {
    println!("Compiling file...");

    let ghc_command = Command::new("ghc")
        .args(["-O0", "-o", executable_path, code_file_path])
        .output()
        .expect("failed to run ghc");

    if !ghc_command.status.success() {
        let mut err = String::from_utf8(ghc_command.stderr)?;
        err = format_haskell_stdout(err);
        error_chain::bail!(err)
    }
    
    Ok(())
}

fn run_file(executable_path : &str) -> Result<String> {
    println!("Running executable...");

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