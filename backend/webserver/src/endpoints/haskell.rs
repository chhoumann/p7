use rocket::State;
use rocket::serde::{json::Json};
use crate::domain::exercise_submission_request::ExerciseSubmissionRequest;
use crate::domain::code_runner_result::CodeRunnerResponse;
use crate::services::test_runner;

#[get("/")]
fn index(runtime: &State<rocket::tokio::runtime::Runtime>) {
    runtime.spawn_blocking(|| {

    });
}

#[post("/haskell", format="json", data = "<exercise_submission_request>")]
pub async fn new(exercise_submission_request: Json<ExerciseSubmissionRequest>, runtime: &State<tokio::runtime::Runtime>) -> Json<CodeRunnerResponse> {
    println!("Spawning thread");

    let res = runtime.spawn_blocking(|| schedule_test(exercise_submission_request))
        .await
        .unwrap()
        .await;

    println!("Ending thread");

    return res
 }


async fn schedule_test(exercise_submission_request: Json<ExerciseSubmissionRequest>) -> Json<CodeRunnerResponse> {
    let exercise_submission = ExerciseSubmissionRequest
    {
        code: exercise_submission_request.code.to_string(),
        test: exercise_submission_request.test.to_string()
    };

    let json_exercise_submission = Json(exercise_submission)
        .into_inner();

    let exercise_code = json_exercise_submission.code;
    let test_code = json_exercise_submission.test;
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