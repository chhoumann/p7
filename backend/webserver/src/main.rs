use std::env;


fn set_env() -> u16{
    let key = "$PORT";

    match env::var(key){
        Ok(val) => val.parse::<u16>().unwrap(),
        _ => 8080
    }
}


#[macro_use] extern crate rocket;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index])
}