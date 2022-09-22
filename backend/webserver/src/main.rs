use rocket::serde::{Deserialize, json::Json};
use std::env;


fn set_env() -> u16{
    let key = "$PORT";

    match env::var(key){
        Ok(val) => val.parse::<u16>().unwrap(),
        _ => 8080
    }
}

#[derive(Deserialize)]
#[serde(crate = "rocket::serde")]
struct Task {
    code: String,
}

#[post("/haskell", format="json", data = "<task>")]
fn new(task: Json<Task>) -> String{ 
     format!("Hello: {}", task.code)
 }

#[macro_use] extern crate rocket;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index]).mount("/haskell", routes![new])
}