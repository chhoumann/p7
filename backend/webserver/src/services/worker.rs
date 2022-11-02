use tokio::sync::mpsc::Receiver;
use uuid::Uuid;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::{runtime, task};

use crate::domain::web_api_data::{ExerciseSubmission, TestRunnerResult, TestRunnerWork};
use crate::test_runner;


pub fn run(
    rx : Receiver<TestRunnerWork>,
    jobs : Arc<Mutex<Box<HashMap<Uuid, Option<TestRunnerResult>>>>>
) {
    tokio::spawn(worker_thread(rx, jobs));
}


pub async fn worker_thread(
    mut rx : Receiver<TestRunnerWork>,
    jobs : Arc<Mutex<Box<HashMap<Uuid, Option<TestRunnerResult>>>>>
) {
    loop {
        let work = rx.recv().await.unwrap();
        let job_handler = jobs.clone();
        
        println!("Running worker thread on UUID {}...", work.id);
        
        task::spawn(async move {
            let res = schedule_test(work.submission).await;
            let mut map = job_handler.lock().unwrap();
            map.insert(work.id, Some(res));
        });
    }
}


pub async fn schedule_test(exercise_submission: ExerciseSubmission) -> TestRunnerResult {
    let exercise_code = exercise_submission.code;
    let test_code = exercise_submission.test;
    let result = test_runner::execute(exercise_code, test_code).await;

    match result {
        Ok(output) => TestRunnerResult {
            success: true,
            output
        },
        Err(error_message) => TestRunnerResult {
            success: false,
            output: error_message.to_string()
        }
    }
}