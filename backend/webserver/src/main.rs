mod domain;
mod endpoints;
mod services;

#[macro_use]
extern crate rocket;
#[launch]
fn rocket() -> _ {
    rocket::build()
        .mount("/", routes![endpoints::index::index])
        .mount("/", routes![endpoints::haskell::new])
        .attach(endpoints::cors::CORS)
}
