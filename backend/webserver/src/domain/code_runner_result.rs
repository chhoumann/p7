use rocket::serde::{Deserialize, Serialize};

#[derive(Deserialize)]
#[derive(Serialize)]
#[serde(crate = "rocket::serde")]
pub struct CodeRunnerResponse {
    pub output: String
}