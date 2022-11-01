use serde::{Serialize, Deserialize};

#[derive(Deserialize)]
#[derive(Serialize)]
pub struct ExerciseSubmissionRequest {
    pub code: String,
    pub test: String,
}
