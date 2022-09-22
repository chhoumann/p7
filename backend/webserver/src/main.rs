use rocket::serde::{Deserialize, json::Json};

#[derive(Deserialize)]
#[serde(crate = "rocket::serde")]
struct Task {
    code: String,
}

#[post("/haskell", format="json", data = "<task>")]
fn new(task: Json<Task>) -> String{ 
     format!("Hello: {:?}", task.code)
 }

#[macro_use] extern crate rocket;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index]).mount("/", routes![new])
}