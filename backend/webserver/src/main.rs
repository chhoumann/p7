use std::sync::Arc;
use axum::{routing::post, Router, Extension};
use axum::body::Body;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Sender, Receiver};

use crate::services::test_runner;
use crate::services::worker;
use crate::domain::web_api_data::{TestRunnerWork};
use crate::domain::shared_state::State;

mod domain;
mod endpoints;
mod services;


#[tokio::main]
async fn main() {
    let (tx, rx) : (Sender<TestRunnerWork>, Receiver<TestRunnerWork>) = mpsc::channel(10); 
    
    let shared_state = Arc::new(State { tx } ) ;
    
    let app : Router<Body> = Router::new()
        .route("/haskell", post(endpoints::haskell::new))
        .layer(Extension(shared_state));
    
    worker::run(rx);

    axum::Server::bind(&"0.0.0.0:8000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

