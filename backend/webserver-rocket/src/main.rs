mod domain;
mod endpoints;
mod services;
mod debug;

#[macro_use]
extern crate rocket;
#[rocket::main]
async fn main() -> Result<(), rocket::Error> {
    println!("main");

    let _rocket = rocket::build()
        .mount("/", routes![endpoints::index::index])
        .mount("/", routes![endpoints::haskell::new])
        .attach(endpoints::cors::CORS)
        .ignite().await?
        .launch().await?;
        
    Ok(())
}
