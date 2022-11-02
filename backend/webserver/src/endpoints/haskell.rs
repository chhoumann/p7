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
    let work = TestRunnerWork { result: None };
    
    state.jobs.lock().unwrap().insert(id, work);
    
    let _res = state.tx.send(exercise_submission).await;

    Json(Token { id })
}


pub async fn get_test_runner_result(
    Path(id) : Path<Uuid>,
    Extension(state): Extension<Arc<State>>,
) -> Json<Value> {
    let map = state.jobs.lock().unwrap();
    
    if !map.contains_key(&id) {
        // UUID not contained in hashmap at all
        return Json(json!({ "status": "not found" }))
    }
    
    let work = map.get(&id).unwrap();
 
    if work.result.is_none() {
        // Work is not completed yet
        return Json(json!({ "status": "in progress" }))
    }

    let result = work.result.clone().unwrap();
    
    Json(json!(result))
}