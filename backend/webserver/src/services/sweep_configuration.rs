use std::time::Duration;


pub struct Configuration {
    pub duration_between_sweeps: Duration,
    pub lifetime: Duration,
}

 
pub fn from_dot_env() -> Configuration {
    let duration_between_sweeps = dotenv::var("DURATION_BETWEEN_SWEEPS")
        .unwrap()
        .parse::<u64>()
        .unwrap();
    
    let lifetime = dotenv::var("REQUEST_LIFETIME")
        .unwrap()
        .parse::<u64>()
        .unwrap();
    
    Configuration { 
        duration_between_sweeps: Duration::from_secs(duration_between_sweeps),
        lifetime: Duration::from_secs(lifetime),
    }
}