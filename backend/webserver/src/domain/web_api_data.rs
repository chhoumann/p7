use serde::{Serialize, Deserialize};
use uuid::Uuid;

#[derive(Deserialize)]
#[derive(Serialize)]
pub struct ExerciseSubmission {
    pub code: String,
    pub test: String,
}

pub struct TestRunnerWork {
    pub result : Option<TestRunnerResult>
}


#[derive(Deserialize)]
#[derive(Serialize)]
pub struct Token {
    pub id : Uuid
}


#[derive(Deserialize)]
#[derive(Serialize)]
#[derive(Clone)]
pub struct TestRunnerResult {
    pub success: bool,
    pub output: String
}
