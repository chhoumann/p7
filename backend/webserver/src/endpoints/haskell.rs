use rocket::serde::{json::Json};
use crate::domain::task::Task;
use crate::services::{code_runner, write_struct};



#[post("/haskell", format="json", data = "<task>")]
pub fn new(task: Json<Task>) -> Json<Task>{ 
    let json_task = Json(Task { code: task.code.to_string() }).into_inner();
    println!("dfsfds");
    let out_file = "../services/haskell-code/gfdjkslghdfskl";
    let _ = write_struct::write_struct(json_task, out_file);
    let res = code_runner::compile_file(out_file, ".hs").unwrap();
    println!("abc");
    

    Json(Task { code: res })
 }

