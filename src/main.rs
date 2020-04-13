use crate::scanner::Scanner;

use crate::parser::Parser;
use crate::token::Token;
use std::collections::HashMap;
use std::fs::{self};
use std::io::{self, Result};
use std::path::Path;

pub mod expression;
pub mod parser;
pub mod scanner;
pub mod statement;
pub mod token;

fn visit_file(path: &Path) -> io::Result<()> {
    if let Some(ext) = path.extension() {
        if ext == "php" {
            let p = path.to_str().unwrap().to_string();

            let content = fs::read_to_string(path)?;
            let mut scanner = Scanner::new(&content);

            if let Err(msg) = scanner.scan() {
                eprintln!("Could not read file {}: {}", &p, &msg);
            }

            // Later on we need to generate an AST, as well as an environment and the
            // symbol table. This will then replace the token streams
            //t.insert(p, scanner.tokens);

            let mut parser = Parser::new(&scanner.tokens);
            let result = parser.ast();

            if let Err(e) = result {
                println!("{}: {:#?}", p, e);
            } else if !parser.errors().is_empty() {
                println!("Errors parsing {}", p);
                println!("Parsed {:#?}", parser.errors());
            } else {
                //println!("{:#?}", result);
            }
            //println!("{:#?}", parser.ast());
            //if let Err(msg) = index_file(&p, file_registry.add(&p), t) {
            //    eprintln!("Could not read file {}: {}", &p, &msg);
            //}
        }
    }

    Ok(())
}

fn visit_dirs(dir: &Path, t: &mut HashMap<String, Vec<Token>>) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                visit_dirs(&path, t)?;
            } else {
                visit_file(&path)?;
            }
        }
    } else {
        visit_file(&dir)?;
    }

    Ok(())
}

fn main() -> Result<()> {
    let mut token_stream_map: HashMap<String, Vec<Token>> = HashMap::new();

    visit_dirs(
        Path::new(&std::env::args().nth(1).unwrap_or(String::from("."))),
        &mut token_stream_map,
    )?;
    println!("Indexed!");

    Ok(())
}
