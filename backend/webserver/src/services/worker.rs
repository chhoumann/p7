use axum::extract::Json;
use tokio::sync::mpsc::Receiver;

use crate::domain::web_api_data::{ExerciseSubmissionRequest, TestRunnerWork, TestRunnerResponse};
use crate::test_runner;

pub fn run(mut rx : Receiver<TestRunnerWork>) {
    tokio::spawn(async move {
        loop {
            let data = rx.recv().await.unwrap();
            println!("{}", data.submission.code);
            
            let res = schedule_test(data.submission).await;
            println!("done, result = {}", res.result);
            
            // TODO: Respond to client here
        }
    });
}

pub async fn schedule_test(exercise_submission: ExerciseSubmissionRequest) -> Json<TestRunnerResponse> {
    let exercise_code = exercise_submission.code;
    let test_code = exercise_submission.test;
    let result = test_runner::execute(exercise_code, test_code).await;

    match result {
        Ok(output) => Json(TestRunnerResponse {
            success: true,
            result: output
        }),
        Err(error_message) => Json(TestRunnerResponse {
            success: false,
            result: error_message.to_string()
        })
    }
}