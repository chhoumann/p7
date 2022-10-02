use rocket::serde::{Deserialize, Serialize};

#[derive(Deserialize)]
#[derive(Serialize)]
#[serde(crate = "rocket::serde")]
pub struct ExerciseSubmissionRequest {
    pub code: String,
}
