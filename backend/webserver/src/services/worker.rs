use std::cmp::max;
use tokio::sync::mpsc::Receiver;
use uuid::Uuid;
use std::collections::HashMap;
use std::ops::Sub;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};
use tokio::task;
use futures::stream::{StreamExt};
use tokio::time::MissedTickBehavior::Delay;

use crate::domain::web_api_data::{ExerciseSubmission, TestRunnerResult, TestRunnerWork};
use crate::test_runner;


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
    
    let duration_between_sweeps = Duration::from_secs(10);
    let mut last_sweep_time = SystemTime::now();
    
    stream.for_each_concurrent(limit, |work| async {
        println!("Running worker thread on UUID {}...", work.id);

        let res = schedule_test(work.submission).await;
        let mut map = job_results.lock().unwrap();
        map.insert(work.id, Some(res));
        
        
        if last_sweep_time.elapsed().unwrap() > duration_between_sweeps {
            last_sweep_time = SystemTime::now();
            clear_old_job_results(job_results);
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


fn clear_old_job_results(job_results: Arc<Mutex<Box<HashMap<Uuid, Option<TestRunnerResult>>>>>) {
    let time_to_live = Duration::from_secs(10);
    let mut map = job_results.lock().unwrap();
    
    map.retain(|&uuid, &res| res.unwrap().timestamp.elapsed() <= &time_to_live);
}