use rocket::serde::{json::Json};
use crate::domain::task::Task;
use crate::domain::code_runner_result::CodeRunnerResult;
use crate::services::{code_runner, write_struct};
use std::path::Path;

const FILE_NAME : &str = "test";
const FILE_EXTENSION : &str = ".hs";

#[post("/haskell", format="json", data = "<task>")]
pub fn new(task: Json<Task>) -> Json<CodeRunnerResult> { 
    let json_task = Json(Task { code: task.code.to_string() }).into_inner();
    let out_file = Path::new("haskell-code").join(format!("{}{}", FILE_NAME, FILE_EXTENSION))
        .into_os_string()
        .into_string()
        .unwrap();
    
    write_struct::write_struct(json_task, &out_file).expect("Could not write code to file!");

    let output = code_runner::compile_file(FILE_NAME, FILE_EXTENSION).unwrap();

    return Json(CodeRunnerResult { output: output })
 }

