use rocket::serde::{json::Json};
use crate::domain::task::Task;
use crate::services::{code_runner, write_struct};



#[post("/haskell", format="json", data = "<task>")]
pub fn new(task: Json<Task>) -> Json<Task>{ 
    let json_task = Json(Task { code: task.code.to_string() }).into_inner();
    let out_file = "haskell-code/test.hs";
    
    write_struct::write_struct(json_task, out_file).expect("Could not write code to file!");

    let res = code_runner::compile_file("test", ".hs").unwrap();

    Json(Task { code: res })
 }

