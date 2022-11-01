use serde::{Serialize, Deserialize};

#[derive(Deserialize)]
#[derive(Serialize)]
pub struct CodeRunnerResponse {
    pub success: bool,
    pub result: String
}