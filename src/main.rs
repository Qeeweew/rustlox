extern crate alloc;
#[macro_use]
extern crate lazy_static;
mod parser2;
mod object;
mod interpreter;
mod resolver;
mod ast;
use std::{io::{self, Read}};
use interpreter::Interpreter;
fn run(s: String) {
    let res = parser2::program(s.as_bytes());
    let mut interpreter = Interpreter::new();
    match res {
        Ok((reamin, ast)) => {
            for stmt in &ast {
                println!("{}", stmt)
            }
            if reamin.len() != 0 {
                println!("parse failed at {}", String::from_utf8(reamin.into()).unwrap())
            } else {
                let res = interpreter.interpret(ast);
                match res {
                    Ok(_) => println!("success!"),
                    Err(e) => println!("{:?}", e),
                };
            }
        }
        Err(e) => println!("{:?}", e),
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
