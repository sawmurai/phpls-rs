#![allow(dead_code)]
#![allow(unused_macros)]

use crate::scanner::Scanner;
use std::collections::HashMap;
use std::fs::File;
use std::fs::{self};
use std::io::{self, BufRead, BufReader, Result};
use std::path::Path;

pub mod scanner;
pub mod token;

type Location = (u16, u16, u16);

#[derive(Default, Debug)]
struct Trie {
    // List of children, by character
    children: HashMap<char, Trie>,

    // List of starting positions of words ending here
    endings: Vec<Location>,
}

#[derive(Default, Debug)]
struct FileRegistry {
    files: Vec<String>,
    size: u16,
}

impl FileRegistry {
    fn add(&mut self, p: &str) -> u16 {
        self.files.push(p.to_owned());

        self.size += 1;

        self.size - 1
    }
}

trait Php {
    fn is_separator(&self) -> bool;
}

impl Php for char {
    fn is_separator(&self) -> bool {
        match self {
            ' ' | '.' | ',' | '[' | ']' | '{' | '}' | '(' | ')' | '+' | '-' | '*' | '/' | ';'
            | '<' | '>' => true,
            _ => false,
        }
    }
}

/// Insert a new token into the suffix trie
fn insert(t: &mut Trie, string: &str, file_registry_index: u16, line: u16, pos: u16) {
    let mut child = t;

    for (_i, c) in string.char_indices() {
        child = child.children.entry(c).or_insert(Trie::default());
    }

    child.endings.push((file_registry_index, line, pos));
}

/// Find the starting positions of all strings. Only find entire words.
fn find<'a>(t: &'a Trie, string: &str) -> Option<&'a Vec<Location>> {
    let mut child = t;

    for (_i, c) in string.char_indices() {
        child = child.children.get(&c)?;
    }

    Some(&child.endings)
}

/// Index a file into the trie and references tokens with the file_registry_index to save some memory
/// over copying the file name over and over again
fn index_file(path: &str, file_registry_index: u16, t: &mut Trie) -> io::Result<()> {
    let mut cc: u16;
    let mut lc: u16 = 0;

    let file = File::open(path)?;
    for line in BufReader::new(file).lines() {
        lc += 1;
        cc = 0;
        let line = line?;

        // TODO: Fix the cc value as we currently do not have a nice index
        for word in line.split(|c: char| c.is_separator()).filter(|s| *s != "") {
            insert(t, &word, file_registry_index, lc, cc);

            cc += word.len() as u16;
        }
    }

    Ok(())
}

fn scan_file(path: &str) -> io::Result<()> {
    let file = File::open(path)?;

    let mut content = fs::read_to_string(path)?;

    let mut scanner = Scanner::new(&content);
    scanner.scan();

    Ok(())
}

fn visit_dirs(dir: &Path, file_registry: &mut FileRegistry, t: &mut Trie) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                visit_dirs(&path, file_registry, t)?;
            } else {
                if let Some(ext) = path.extension() {
                    if ext == "php" {
                        let p = path.to_str().unwrap().to_string();

                        if let Err(msg) = scan_file(&p) {
                            eprintln!("Could not read file {}: {}", &p, &msg);
                        }
                        //if let Err(msg) = index_file(&p, file_registry.add(&p), t) {
                        //    eprintln!("Could not read file {}: {}", &p, &msg);
                        //}
                    }
                }
            }
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let mut file_registry = FileRegistry::default();
    let mut t = Trie::default();

    visit_dirs(
        Path::new("/home/fbecker/develop/web/unity/unity-platforms"),
        &mut file_registry,
        &mut t,
    )?;
    println!("Indexed!");
    let stdin = io::stdin();

    for line in stdin.lock().lines() {
        let line = line?;

        if line == "" {
            break;
        }
        if let Some(results) = find(&t, &line) {
            for (path_index, line, pos) in results {
                let f = file_registry.files.get(*path_index as usize);
                println!("{}:{}:{}", f.unwrap(), line, pos);
            }
        } else {
            println!("No results.")
        }
    }
    Ok(())
}
