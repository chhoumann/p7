use rocket::serde::{Deserialize, json::Json, Serialize};
use rocket::fairing::{Fairing, Info, Kind};
use rocket::http::Header;
use rocket::{Request, Response};

pub struct CORS;

#[rocket::async_trait]
impl Fairing for CORS {
    fn info(&self) -> Info {
        Info {
            name: "Add CORS headers to responses",
            kind: Kind::Response
        }
    }

    async fn on_response<'r>(&self, _request: &'r Request<'_>, response: &mut Response<'r>) {
        response.set_header(Header::new("Access-Control-Allow-Origin", "*"));
        response.set_header(Header::new("Access-Control-Allow-Methods", "POST, GET, PATCH, OPTIONS"));
        response.set_header(Header::new("Access-Control-Allow-Headers", "*"));
        response.set_header(Header::new("Access-Control-Allow-Credentials", "true"));
    }
}

#[derive(Deserialize)]
#[derive(Serialize)]
#[serde(crate = "rocket::serde")]
struct Task {
    code: String,
}

#[post("/haskell", format="json", data = "<task>")]
fn new(task: Json<Task>) -> Json<Task>{ 
    let object = task;
    let res = Json(Task { code: object.code.to_string() });
    
    // This struct will contain the result of parsing the json:
    println!("{}", res.code);

    return res
 }

#[macro_use] extern crate rocket;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index]).mount("/", routes![new]).attach(CORS)
}