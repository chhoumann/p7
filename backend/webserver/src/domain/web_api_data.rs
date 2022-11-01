use serde::{Serialize, Deserialize};

#[derive(Deserialize)]
#[derive(Serialize)]
pub struct ExerciseSubmissionRequest {
    pub code: String,
    pub test: String,
}

pub struct TestRunnerWork {
    pub submission : ExerciseSubmissionRequest,
    // We probably eventually need to add the client's information here
}

#[derive(Deserialize)]
#[derive(Serialize)]
pub struct TestRunnerResponse {
    pub success: bool,
    pub result: String
}