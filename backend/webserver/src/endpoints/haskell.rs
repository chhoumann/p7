use axum::Extension;
use axum::extract::Json;
use std::sync::Arc;
use uuid::Uuid;

use crate::domain::shared_state::State;
use crate::domain::web_api_data::{ExerciseSubmission, TestRunnerWork, Token};


pub async fn new(
    Json(exercise_submission): Json<ExerciseSubmission>,
    Extension(state): Extension<Arc<State>>,
) -> Json<Token> {
    let id = Uuid::new_v4();
    
    let work = TestRunnerWork {
        complete: false,
        result: None
    };
    
    state.jobs.lock().unwrap().insert(id, work);
    
    let _res = state.tx.send(exercise_submission).await;

    // TODO: Actually send back the result of the test runner (web socket?)
    Json(Token { id })
}