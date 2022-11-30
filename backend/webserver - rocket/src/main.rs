mod domain;
mod endpoints;
mod services;
mod debug;

#[macro_use]
extern crate rocket;
#[launch]
#[rocket::main]
async fn main() -> Result<(), rocket::Error> {
    rocket::build()
        .mount("/", routes![endpoints::index::index]).await?
        .mount("/", routes![endpoints::haskell::new]).await?
        .attach(endpoints::cors::CORS);
        
    Ok(())
}
