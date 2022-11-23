use tokio::fs;
use std::path::Path;

const CONTAINER_DIR_NAME : &str = "haskell-code";

pub async fn create_container_dir() {
    if Path::new(CONTAINER_DIR_NAME).is_dir() {
        fs::remove_dir_all(CONTAINER_DIR_NAME).await.unwrap();
    }
    
    fs::create_dir(CONTAINER_DIR_NAME)
        .await
        .expect(&format!("Could not create directory \"{}\"!", CONTAINER_DIR_NAME));
}


pub async fn generate_dir() -> String {
    let dir_name = generate_code_dir_name();

    fs::create_dir(&dir_name)
        .await
        .expect(&format!("Could not create directory \"{}\"!", dir_name));

    dir_name
}


fn generate_code_dir_name() -> String {
    let mut dir_name : String;

    loop {
        let id = rand::random::<u32>()
            .to_string();

        dir_name = Path::new(CONTAINER_DIR_NAME)
            .join(id)
            .into_os_string()
            .into_string()
            .unwrap();
        
         if !(Path::new(&dir_name)).is_dir() {
            break;
         }
    }

    dir_name
}
