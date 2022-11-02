use tokio::sync::mpsc::Receiver;
use uuid::Uuid;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::domain::web_api_data::{ExerciseSubmission, TestRunnerResult, TestRunnerWork};
use crate::test_runner;

pub fn run(
    mut rx : Receiver<TestRunnerWork>,
    jobs : Arc<Mutex<Box<HashMap<Uuid, Option<TestRunnerResult>>>>>
) {
    // TODO: Use a task instead
    tokio::spawn(async move {
        loop {
            let work = rx.recv().await.unwrap();
            let res = schedule_test(work.submission).await;
            
            println!("done, result = {}, id = {}", res.output, work.id);

            let mut map = jobs.lock().unwrap();
            map.insert(work.id, Some(res));
        }
    });
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