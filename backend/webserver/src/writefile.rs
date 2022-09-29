use std::fs::File;
use std::io::prelude::*;

fn writeStruct(exercise : Task) -> std::io::Result<()>{    
    let mut file = File::create("helloHaskell.hs")?;
    file.write_all(exercise.code.as_bytes())?;
    Ok(())
}