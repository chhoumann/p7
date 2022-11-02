use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use axum::{routing::post, Router, Extension};
use axum::body::Body;
use axum::routing::get;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Sender, Receiver};

use crate::services::test_runner;
use crate::services::worker;
use crate::domain::web_api_data::TestRunnerWork;
use crate::domain::shared_state::State;

mod domain;
mod endpoints;
mod services;


#[tokio::main]
async fn main() {
    let (tx, rx) : (Sender<TestRunnerWork>, Receiver<TestRunnerWork>) = mpsc::channel(10); 
    let map = Arc::new(Mutex::new(Box::new(HashMap::new())));
    
    let shared_state = Arc::new(State {
        tx,
        jobs: map.clone()
    });
    
    let app : Router<Body> = Router::new()
        .route("/haskell/submit", post(endpoints::haskell::submit))
        .route("/haskell/getResult/:id", get(endpoints::haskell::get_test_runner_result))
        .layer(Extension(shared_state));

    worker::run(rx, map);

    // TODO: Make the IP address and port use environment variables
    axum::Server::bind(&"0.0.0.0:8000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

