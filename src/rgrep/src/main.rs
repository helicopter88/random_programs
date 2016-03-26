use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::env;
use std::fs;
use std::thread;
use std::sync::Arc;

fn grep_file(file: std::path::PathBuf, pattern: &str) -> Result<(), io::Error> {
    // We can use try here as this function returns a Result<>
    let f = try!(File::open(&file));
    let reader = BufReader::new(f);
    for (number, line) in reader.lines().enumerate() {
        let line = try!(line);
        if line.contains(&pattern) {
            println!("{}:{} -> {}",file.display() ,number + 1, line);
        }
    }
    // Close the file
    drop(&file);
    Ok(())
}

fn main() {
    let mut args = env::args();
    if args.len() == 3 {
        let mut threads = Vec::new();
        // Consume the first 2 arguments.
        // We know this is not going to fail
        let path = &args.nth(1).unwrap();
        // Get the last one, that is the pattern
        let p = args.nth(0).unwrap().clone();
        // Arc ensures proper synchronization between threads
        let pattern = Arc::new(p.clone());
        // Get each file in the given path
        for file in fs::read_dir(path).unwrap() {
            let ph = pattern.clone();
            // Store each thread into the vector so that we can wait for them later
            threads.push(thread::spawn(move || {
                        let _ = grep_file(file.unwrap().path(), &ph);
                    })
            );
        }

        // Wait for every thread to finish
        for thr in threads {
            let _ = thr.join();
        }
    } else {
        println!("Expected 2 arguments, got {}", args.len())
    }
}
