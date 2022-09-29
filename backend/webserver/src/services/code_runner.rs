use std::env;
use std::path::Path;
use std::process::Command;
use error_chain::error_chain;

const DIRECTORY_NAME : &str = "haskell-code";

error_chain!{
    errors { CmdError }
    foreign_links {
        Io(std::io::Error);
        Utf8(std::string::FromUtf8Error);
    }
}

/// Compiles and runs the given file in the "haskell-directory" folder.
/// 
/// # Arguments
/// * `file_name` - The name of the file (without file extension!).
/// * `file_extension` - The file extension (i.e. .hs).
/// 
/// # Examples
/// ```
/// let res = code_runner::compile_file("test", ".hs").unwrap();
/// ```
pub fn compile_file(file_name : &str, file_extension : &str) -> Result<String> {
    let file_name_with_extension = format!("{}{}", file_name, file_extension);
    let file_path = Path::new(DIRECTORY_NAME).join(file_name_with_extension).into_os_string().into_string().unwrap();
    let output_path = Path::new(DIRECTORY_NAME).join(file_name).into_os_string().into_string().unwrap();

    let ghc_command = Command::new("ghc")
        .args(["-o", &output_path, &file_path])
        .output()
        .expect("failed to run ghc");
    
    if !ghc_command.status.success() {
        let err = String::from_utf8(ghc_command.stderr)?;
        error_chain::bail!("Failed to run ghc:\n {}", err)
    }
    
    let current_dir = env::current_dir()?;
    let executable_path = Path::new(&current_dir).join(DIRECTORY_NAME).join(file_name)
        .into_os_string()
        .into_string()
        .unwrap();

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