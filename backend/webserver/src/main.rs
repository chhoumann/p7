use actix_web::{web, App, HttpServer, Result};
use serde::Deserialize;
use std::env;


fn set_env() -> u16{
    let key = "$PORT";

    match env::var(key){
        Ok(val) => val.parse::<u16>().unwrap(),
        _ => 8080
    }
}


#[derive(Deserialize)]
struct Code {
    code: String,
}

async fn index(code: web::Json<Code>) -> Result<String> {
    Ok(format!("Welcome {}!", code.code))
}


#[actix_web::main]
async fn main() -> std::io::Result<()> {

    let env_port = set_env();
    HttpServer::new(|| App::new().route("/haskell", web::post().to(index)))
        .bind(("127.0.0.1", env_port))?
        .run()
        .await
}