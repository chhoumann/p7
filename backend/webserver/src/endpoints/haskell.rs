use rocket::serde::{json::Json};
use crate::domain::exercise_submission_request::ExerciseSubmissionRequest;
use crate::domain::code_runner_result::CodeRunnerResponse;
use crate::services::code_runner;


#[post("/haskell", format="json", data = "<exercise_submission_request>")]
pub fn new(exercise_submission_request: Json<ExerciseSubmissionRequest>) -> Json<CodeRunnerResponse> { 
    let exercise_submission = ExerciseSubmissionRequest 
    {
         code: exercise_submission_request.code.to_string() 
    };
    
    let json_exercise_submission = Json(exercise_submission)
        .into_inner();
    
    let code = json_exercise_submission.code;
    let result = code_runner::execute(code, String::from("This is the test"));

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

