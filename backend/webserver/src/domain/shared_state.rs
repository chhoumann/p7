use crate::domain::web_api_data::TestRunnerWork;
use tokio::sync::mpsc::Sender;

pub struct State {
    pub tx : Sender<TestRunnerWork>
}
