mod domain;
mod endpoints;
mod services;

#[macro_use]
extern crate rocket;

#[rocket::main]
async fn main() -> Result<(), rocket::Error> {
    let runtime = rocket::tokio::runtime::Builder::new_multi_thread()
        .max_blocking_threads(2)
        .enable_all()
        .build()
        .unwrap();

    let _rocket = rocket::build()
        .manage(runtime)
        .mount("/", routes![endpoints::index::index])
        .mount("/", routes![endpoints::haskell::new])
        .attach(endpoints::cors::CORS)
        .launch()
        .await?;

    Ok(())
}
