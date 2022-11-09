use tokio::sync::mpsc::Receiver;
use uuid::Uuid;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;
use tokio::task;
use futures::stream::StreamExt;

use crate::domain::web_api_data::{ExerciseSubmission, TestRunnerResult, TestRunnerWork};
use crate::test_runner;
use crate::services::sweep_configuration;


pub fn run(
    rx : Receiver<TestRunnerWork>,
    job_results: Arc<Mutex<Box<HashMap<Uuid, Option<TestRunnerResult>>>>>,
    limit : usize
) {
    task::spawn(worker_thread(rx, job_results, limit));
}


async fn worker_thread(
    rx: Receiver<TestRunnerWork>,
    job_results: Arc<Mutex<Box<HashMap<Uuid, Option<TestRunnerResult>>>>>,
    limit : usize
) {
    let stream = tokio_stream::wrappers::ReceiverStream::new(rx);
    let last_sweep_time = Arc::new(Mutex::new(SystemTime::now()));
    let config = sweep_configuration::from_dot_env();
    
    stream.for_each_concurrent(limit, |work| async {
        println!("Running worker thread on UUID {}...", work.id);

        let res = schedule_test(work.submission).await;
        let mut map = job_results.lock().unwrap();
        map.insert(work.id, Some(res));

        if last_sweep_time.lock().unwrap().elapsed().unwrap() > config.duration_between_sweeps {
            // Sweep old requests
            println!("Sweeping now...");

            map.retain(|&_, res| {
                res.as_ref().unwrap().timestamp.elapsed().unwrap() <= config.lifetime
            });
            
            *last_sweep_time.lock().unwrap() = SystemTime::now();
        }
    }).await;
}


pub async fn schedule_test(exercise_submission: ExerciseSubmission) -> TestRunnerResult {
    let exercise_code = exercise_submission.code;
    let test_code = exercise_submission.test;
    let result = test_runner::execute(exercise_code, test_code).await;
    let timestamp = SystemTime::now();
    
    match result {
        Ok(output) => TestRunnerResult {
            success: true,
            output,
            timestamp
        },
        Err(error_message) => TestRunnerResult {
            success: false,
            output: error_message.to_string(),
            timestamp
        }
    }
}
