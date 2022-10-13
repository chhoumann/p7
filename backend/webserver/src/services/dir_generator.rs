use std::fs;
use std::path::Path;

const CONTAINER_DIR_NAME : &str = "haskell-code";

pub fn generate_dir() -> String {
    if !Path::new(CONTAINER_DIR_NAME).is_dir() {
        fs::create_dir(CONTAINER_DIR_NAME)
            .expect(&format!("Could not create directory \"{}\"!", CONTAINER_DIR_NAME));
    }

    return create_code_directory()
}

fn create_code_directory() -> String {
    let dir_name = create_code_dir();
    
    fs::create_dir(&dir_name)
        .expect(&format!("Could not create directory \"{}\"!", dir_name));

    return dir_name
}

fn create_code_dir() -> String {
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

    return dir_name
}
