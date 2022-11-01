use axum::Extension;
use axum::extract::Json;
use std::sync::Arc;

use crate::domain::shared_state::State;
use crate::domain::web_api_data::{ExerciseSubmissionRequest, TestRunnerWork, TestRunnerResponse};


pub async fn new(
    Json(exercise_submission): Json<ExerciseSubmissionRequest>,
    Extension(state): Extension<Arc<State>>,
) -> Json<TestRunnerResponse> {
    
    let work = TestRunnerWork { submission: exercise_submission };
    let _res = state.tx.send(work).await;

    Json(TestRunnerResponse {
        success: true,
        result: String::from("ok k")
    })
}