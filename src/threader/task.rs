use super::files::*;
use crate::{error::CliError, format_time, parser::parse};
use colored::Colorize;
use std::{
    ffi::OsString,
    fs::*,
    path::{Path, PathBuf},
    time::{Instant, UNIX_EPOCH},
};

#[derive(Clone, Debug)]
pub enum Task {
    ReadDir { path: PathBuf },
    Parse { path: &'static Path },
}

impl Task {
    pub(super) fn run<F>(self, mut push: F) -> Result<(), CliError>
    where
        F: FnMut(Task),
    {
        match self {
            Task::ReadDir { path } => {
                let mut relevant_files = Directory::new();
                for entry in read_dir(&path)? {
                    let Ok(entry) = entry else { continue };
                    let path = entry.path();
                    let name: String;
                    if let Some(os_str) = path.file_name() {
                        if let Some(str_name) = os_str.to_str() {
                            match str_name.strip_prefix(".") {
                                Some(str) => name = String::from(str),
                                None => name = String::from(str_name),
                            }
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }

                    if path.is_dir() {
                        push(Task::ReadDir { path });
                    } else if let Some(extension) = path.extension() {
                        let Ok(last_changed) =
                            path.metadata().and_then(|metadata| metadata.modified())
                        else {
                            continue;
                        };

                        if extension == COMPILED_FILE_EXTENSION {
                            relevant_files.insert_object_file(
                                name.strip_suffix(&COMPILED_FILE_EXTENSION)
                                    .unwrap()
                                    .to_string(),
                                last_changed,
                            )
                        } else if extension == FILE_EXTENSION {
                            relevant_files.insert_project_file(
                                name.strip_suffix(&FILE_EXTENSION).unwrap().to_string(),
                                last_changed,
                            )
                        }
                    }
                }
                for file in relevant_files.files.into_iter() {
                    if file.1 != UNIX_EPOCH {
                        println!(
                            "wants to remove: {:?} name: {}",
                            path.join(String::from(".") + &file.0 + COMPILED_FILE_EXTENSION),
                            file.0
                        );
                        // match fs::remove_file(&path.join(file)) {
                        //     Ok(_) => {},
                        //     Err(_) => println!("You shall not remove that file! file: {:?}", path)
                        // }
                    } else {
                        push(Task::Parse {
                            path: Box::leak(
                                path.join(PathBuf::from(file.0 + FILE_EXTENSION))
                                    .into_boxed_path(),
                            ),
                        });
                    }
                }
            }
            Task::Parse { path } => {
                if path.file_name() != Some(&OsString::from("inter.flou")) {
                    return Ok(());
                }
                let now = Instant::now();
                let (ast, errors) = parse(&read_to_string(path)?, path);
                println!(
                    "{}\n{}\n\nparsed in: {}",
                    *errors,
                    ast,
                    format_time(now.elapsed().as_nanos()).bold(),
                );
            }
        }
        Ok(())
    }

    pub fn read_dir(path: Option<String>) -> Result<Task, CliError> {
        let path = match path {
            // determine path:
            Some(path) => {
                let path = PathBuf::from(path);
                if !path.exists() {
                    return Err(CliError::CommandLine(
                        "the given path seems to be not existing",
                    ));
                }
                if !path.is_dir() {
                    return Err(CliError::CommandLine(
                        "the given path seems to not be a directory",
                    ));
                }
                path
            }
            None => PathBuf::from("."),
        };
        Ok(Task::ReadDir { path })
    }
}
