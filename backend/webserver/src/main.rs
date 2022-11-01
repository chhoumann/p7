use axum::{
    routing::post,
    Router,
};
use axum::body::Body;

mod domain;
mod endpoints;
mod services;

#[tokio::main]
async fn main() {
    let app : Router<Body> = Router::new()
        .route("/haskell", post(endpoints::haskell::new));

    axum::Server::bind(&"0.0.0.0:8000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
