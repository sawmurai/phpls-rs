use std::{fs, path::PathBuf};
use tokio::io::{self};

pub(crate) fn reindex_folder(dir: &PathBuf) -> io::Result<Vec<PathBuf>> {
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
            if path.is_dir() {
                // && !path.ends_with("vendor") {
                files.extend(reindex_folder(&path)?);
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
