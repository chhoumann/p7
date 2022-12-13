use axum::Extension;
use axum::extract::{Json, Path};
use std::sync::Arc;
use uuid::Uuid;
use serde_json::{Value, json};

use crate::domain::shared_state::State;
use crate::domain::web_api_data::{ExerciseSubmission, TestRunnerWork, Token};


pub async fn submit(
    Json(exercise_submission): Json<ExerciseSubmission>,
    Extension(state): Extension<Arc<State>>,
) -> Json<Token> {
    let id = Uuid::new_v4();
    let work = TestRunnerWork { 
        id,
        submission: exercise_submission,
        result: None 
    };

    state.job_results.lock().unwrap().insert(id, None);
    
    let _res = state.tx.send(work).await;

    Json(Token { id })
}


pub async fn get_test_runner_result(
    Path(id) : Path<Uuid>,
    Extension(state): Extension<Arc<State>>,
) -> Json<Value> {
    let map = state.job_results.lock().unwrap();
    
    if !map.contains_key(&id) {
        // UUID not contained in hashmap at all
        return Json(json!({ "status": "not found" }))
    }
    
    let work = map.get(&id).unwrap();
 
    if work.is_none() {
        // Work is not completed yet
        return Json(json!({ "status": "in progress" }))
    }

    let result = work.clone().unwrap();

    Json(json!({
        "status": "complete",
        "success": result.success,
        "output": result.output
    }))
}