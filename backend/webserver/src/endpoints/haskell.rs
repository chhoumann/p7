use axum::extract::Json;

use crate::domain::exercise_submission_request::ExerciseSubmissionRequest;
use crate::domain::code_runner_result::CodeRunnerResponse;
use crate::services::test_runner;

const MAX_TEST_COUNT: i32 = 10;

static mut COUNT : i32 = 0;

pub async fn new(Json(exercise_submission): Json<ExerciseSubmissionRequest>) -> Json<CodeRunnerResponse> {
    unsafe {
        while COUNT >= MAX_TEST_COUNT {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
        }

        println!("Spawning thread");
        COUNT += 1;

        let res = schedule_test(exercise_submission).await;

        COUNT -= 1;
        println!("Ending thread");

        return res
    }
}


async fn schedule_test(exercise_submission: ExerciseSubmissionRequest) -> Json<CodeRunnerResponse> {
    let exercise_code = exercise_submission.code;
    let test_code = exercise_submission.test;
    let result = test_runner::execute(exercise_code, test_code).await;

    let output = match result {
        Ok(output) => Json(CodeRunnerResponse {
            success: true,
            result: output
        }),
        Err(error_message) => Json(CodeRunnerResponse {
            success: false,
            result: error_message.to_string()
        })
    };

    return output;
}