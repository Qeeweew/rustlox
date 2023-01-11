#[macro_use]
extern crate lazy_static;
mod scanner;
mod token;
use std::{io::{self, Read}};
use scanner::{Scanner};
fn run(s: String) {
    let mut sc = Scanner::new(s);
    let res = sc.scan();
    match res {
        Ok((_, vec)) => println!("{:#?}", vec),
        Err(e) => println!("{:?}",e),
    }
}
fn run_prompt() -> io::Result<()> {
    let stdin = io::stdin();
    loop {
        let mut line = String::new();
        stdin.read_line(&mut line)?;
        if line == "#stop" {
            break;
        }
        run(line);
    };
    Ok(())
}
fn run_file(path: &str) -> io::Result<()> {
    let mut file = std::fs::File::open(path)?;
    let mut buf: String = String::new(); 
    file.read_to_string(&mut buf)?;
    run(buf);
    Ok(())
}
fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 2 {
        println!("Usage: jok [script]");
        std::process::exit(64);
    } else if args.len() == 2 {
        run_file(&args[1])?;
    } else {
        run_prompt()?;
    }
    Ok(())
}
