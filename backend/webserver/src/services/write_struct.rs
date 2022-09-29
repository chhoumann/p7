use std::fs::File;
use std::io::prelude::*;
use crate::domain::task::Task;

pub fn write_struct(exercise : Task, file_name: &str) -> std::io::Result<()> { 
    let mut file = File::create(file_name)?;
    file.write_all(exercise.code.as_bytes())?;
    Ok(())
}