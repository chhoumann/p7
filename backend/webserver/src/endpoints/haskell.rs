use rocket::serde::{json::Json};
use crate::domain::task::Task;
use crate::services::{code_runner, write_struct};
use rocket::serde::{Deserialize, Serialize};

#[derive(Deserialize)]
#[derive(Serialize)]
#[serde(crate = "rocket::serde")]
pub struct CodeRunnerResult {
    pub output: String
}

#[post("/haskell", format="json", data = "<task>")]
pub fn new(task: Json<Task>) -> Json<CodeRunnerResult>{ 
    let json_task = Json(Task { code: task.code.to_string() }).into_inner();
    let out_file = "haskell-code/test.hs";
    
    write_struct::write_struct(json_task, out_file).expect("Could not write code to file!");

    let output = code_runner::compile_file("test", ".hs").unwrap();

    return Json(CodeRunnerResult { output: output })
 }

