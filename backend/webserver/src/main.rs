use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use axum::{routing::post, Router, Extension};
use axum::body::Body;
use axum::routing::get;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Sender, Receiver};
use dotenv::dotenv;
use uuid::Uuid;

use crate::services::test_runner;
use crate::services::worker;
use crate::domain::web_api_data::{TestRunnerResult, TestRunnerWork};
use crate::domain::shared_state::State;

mod domain;
mod endpoints;
mod services;


#[tokio::main]
async fn main() {
    dotenv().ok();
    
    let (tx, rx) = create_channel(); 
    let jobs = Arc::new(Mutex::new(Box::new(HashMap::new())));
    let shared_state = Arc::new(State {
        tx,
        jobs: jobs.clone()
    });
    
    let app = create_app(shared_state);

    run_worker(rx, jobs);
    bind_server(app).await;
}


fn create_channel() -> (Sender<TestRunnerWork>, Receiver<TestRunnerWork>) {
    let buffer_capacity = dotenv::var("CHANNEL_BUFFER_CAPACITY").unwrap().parse::<usize>().unwrap();
    let (tx, rx): (Sender<TestRunnerWork>, Receiver<TestRunnerWork>) = mpsc::channel(buffer_capacity);
    (tx, rx)
}


fn create_app(shared_state: Arc<State>) -> Router {
    let app: Router<Body> = Router::new()
        .route("/haskell/submit", post(endpoints::haskell::submit))
        .route("/haskell/getResult/:id", get(endpoints::haskell::get_test_runner_result))
        .layer(Extension(shared_state));
    app
}


fn run_worker(rx: Receiver<TestRunnerWork>, map: Arc<Mutex<Box<HashMap<Uuid, Option<TestRunnerResult>>>>>) {
    let limit = dotenv::var("MAX_THREADS").unwrap().parse::<usize>().unwrap();
    worker::run(rx, map, limit);
}


async fn bind_server(app: Router) {
    let port = dotenv::var("PORT").unwrap();
    let ip = dotenv::var("IP").unwrap();
    let addr = format!("{}:{}", ip, port);

    axum::Server::bind(&addr.parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

