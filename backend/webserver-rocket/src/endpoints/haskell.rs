use rocket::serde::{json::Json};
use crate::domain::exercise_submission_request::ExerciseSubmissionRequest;
use crate::domain::code_runner_result::CodeRunnerResponse;
use crate::services::code_runner;


#[post("/haskell/submit", format="json", data = "<exercise_submission_request>")]
pub fn new(exercise_submission_request: Json<ExerciseSubmissionRequest>) -> Json<CodeRunnerResponse> { 
    let exercise_submission = ExerciseSubmissionRequest 
    {
         code: exercise_submission_request.code.to_string(),
         test: exercise_submission_request.test.to_string()
    };


    let json_exercise_submission = Json(exercise_submission)
        .into_inner();

    let exercise_code = json_exercise_submission.code;
    let test_code = json_exercise_submission.test;
    let result = code_runner::execute(exercise_code, test_code);

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

    return output
 }

