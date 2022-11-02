use std::collections::HashMap;
use crate::domain::web_api_data::{TestRunnerResult, TestRunnerWork};
use tokio::sync::mpsc::Sender;
use uuid::Uuid;
use std::sync::{Arc, Mutex};


pub struct State {
    pub tx : Sender<TestRunnerWork>,
    pub jobs: Arc<Mutex<Box<HashMap<Uuid, Option<TestRunnerResult>>>>>,
}
