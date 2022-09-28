use error_chain::error_chain;
use std::collections::HashSet;
use std::io::Write;
use std::env;
use std::path::Path;
use std::process::{Command, Stdio};


error_chain!{
    errors { CmdError }
    foreign_links {
        Io(std::io::Error);
        Utf8(std::string::FromUtf8Error);
    }
}

fn main() -> Result<()> {
    let mut ghc = Command::new("ghc").stdin(Stdio::piped())
        .args(["-o", "test", "test.hs"])
        .spawn()?;
        //.stderr(Stdio::piped())
        //.stdout(Stdio::piped())
    
    ghc.wait();

    let current_dir = env::current_dir()?;
    let path = Path::new(&current_dir).join("test.exe")
        .into_os_string()
        .into_string()
        .unwrap();

    let run_command = Command::new(path)
        .output()
        .expect("failed to execute process");
    
    let output = run_command.stdout;
    let raw_output = String::from_utf8(output)?;
    let words = raw_output.split_whitespace()
        .map(|s| s.to_lowercase())
        .collect::<HashSet<_>>();
    
    println!("{}", raw_output);
    // println!("Found {} unique words:", words.len());
    // println!("{:#?}", words);
    
    Ok(())
}

