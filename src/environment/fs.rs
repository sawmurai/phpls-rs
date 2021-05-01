use std::{fs, path::Path, path::PathBuf};
use tokio::io;

pub(crate) fn reindex_folder(dir: &Path, ignore: &[PathBuf]) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    if dir.is_dir() {
        let entries = match fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(e) => {
                eprintln!("Error reading folder {:?}: {}", dir, e);

                return Ok(files);
            }
        };

        for entry in entries {
            let entry = match entry {
                Ok(entry) => entry,
                Err(e) => {
                    eprintln!("- Error reading folder {:?}: {}", dir, e);

                    return Ok(files);
                }
            };
            let path = entry.path();
            if ignore.iter().any(|s| path.ends_with(s)) {
                continue;
            }

            if path.is_dir() {
                // && !path.ends_with("vendor") {
                files.extend(reindex_folder(&path, ignore)?);
            } else if let Some(ext) = path.extension() {
                if ext == "php" {
                    files.push(path);
                }
            }
        }
    }
    Ok(files)
}

pub(crate) fn normalize_path(path: &PathBuf) -> String {
    path.to_str().unwrap().to_owned()
}

pub(crate) fn file_read_range(path: &str, start: u64, end: u64) -> String {
    let content = match fs::read_to_string(path) {
        Ok(content) => content,
        _ => return String::from("Error reading source file"),
    };

    content
        .lines()
        .skip(start as usize)
        .take((end - start + 1) as usize)
        .map(|s| format!("{}\n", s))
        .collect()
}
