use axum::extract::Json;
use tokio::sync::mpsc::Receiver;

use crate::domain::web_api_data::{ExerciseSubmission, TestRunnerResult};
use crate::test_runner;

pub fn run(mut rx : Receiver<ExerciseSubmission>) {
    // TODO: Use a task instead
    tokio::spawn(async move {
        loop {
            let data = rx.recv().await.unwrap();
            println!("{}", data.code);
            
            let res = schedule_test(data).await;
            println!("done, result = {}", res.output);
            
            // TODO: Respond to client here
        }
    });
}

pub async fn schedule_test(exercise_submission: ExerciseSubmission) -> Json<TestRunnerResult> {
    let exercise_code = exercise_submission.code;
    let test_code = exercise_submission.test;
    let result = test_runner::execute(exercise_code, test_code).await;

    match result {
        Ok(output) => Json(TestRunnerResult {
            success: true,
            output: output
        }),
        Err(error_message) => Json(TestRunnerResult {
            success: false,
            output: error_message.to_string()
        })
    }
}