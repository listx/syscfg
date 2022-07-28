use clap::{Arg, Command};
use reqwest::blocking::Client;
use serde::Deserialize;
use serde_json::json;

use std::env;
use std::fs;

mod settings;
use settings::Settings;

#[derive(Deserialize, Debug)]
struct PathShortened {
    path_shortened: String,
}

#[derive(Deserialize, Debug)]
struct PathsSorted {
    paths_sorted: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let settings = Settings::new().unwrap();

    let cmd = clap::Command::new("lhc")
        .version("v0.0")
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("prompt-info")
                .about("Get information to display for a shell prompt.")
                .arg(
                    Arg::new("path")
                        .help("the path to the PWD of the shell instance")
                        .index(1)
                        .required(true),
                )
                .arg(
                    Arg::new("path_aliases_file")
                        .long("path-aliases")
                        .value_name("FILE")
                        .help("File containing path aliases")
                        .takes_value(true)
                        .required(true),
                ),
        )
        .subcommand(
            Command::new("git-info")
                .about("Get Git information (to be used for a shell prompt).")
                .arg(
                    Arg::new("git_repo_path")
                        .help("the path to the Git repository")
                        .index(1)
                        .required(true),
                ),
        )
        .subcommand(
            Command::new("path-shorten")
                .about("Shorten a path")
                .arg(
                    Arg::new("path_to_shorten")
                        .help("the path to shorten")
                        .index(1)
                        .required(true),
                )
                .arg(
                    Arg::new("path_aliases_file")
                        .long("path-aliases")
                        .value_name("FILE")
                        .help("File containing path aliases")
                        .takes_value(true)
                        .required(true),
                ),
        )
        .subcommand(
            Command::new("paths-sort")
                .about("Sort the $PATH")
                .arg(
                    Arg::new("paths_to_sort")
                        .help("the paths to sort ($PATH)")
                        .index(1)
                        .required(true),
                )
                .arg(
                    Arg::new("priorities_raw_file")
                        .long("priorities-raw")
                        .value_name("FILE")
                        .help("File containing path priorities (for sorting $PATH)")
                        .takes_value(true)
                        .required(true),
                ),
        )
        .subcommand(Command::new("ping").about("Check lh server connectivity"))
        .subcommand(Command::new("shutdown").about("Shut down lh server instance"));
    let matches = cmd.get_matches();
    let client = Client::new();
    let request_base_url = format!("http://{}:{}", settings.server.domain, settings.server.port);
    let home_dir = env::var("HOME").expect("$HOME not set");
    match matches.subcommand_name() {
        Some("prompt-info") => {
            if let Some(m) = matches.subcommand_matches("prompt-info") {
                let path = m.value_of("path").unwrap();
                let path_aliases_file = m.value_of("path_aliases_file").unwrap();
                let path_aliases_contents =
                    fs::read_to_string(path_aliases_file).unwrap_or_default();

                let request_url = format!("{}/prompt-info", request_base_url);

                let json_body = json!({
                    "path": path,
                    "aliases": path_aliases_contents,
                });

                let response = client.post(request_url).json(&json_body).send()?;

                let prompt: lh_common::Prompt = response.json()?;

                let mut prompt_git = prompt.git_repo_stats.oneline();
                prompt_git = match prompt.git_repo_stats.status.as_str() {
                    "FINISHED" => format!("[{}] ", prompt_git),
                    "LOADING" => "[...] ".to_string(),
                    _ => "".to_string(),
                };

                print!(
                    "global_prompt_git=\"{}\"
global_prompt_path_short=\"%B%F{{cyan}}{}%f%b\"",
                    prompt_git, prompt.path_short
                );
            }
        }
        Some("git-info") => {
            if let Some(m) = matches.subcommand_matches("git-info") {
                let git_repo_path = m.value_of("git_repo_path").unwrap();

                let request_url = format!("{}/git-info", request_base_url);

                let json_body = json!({
                    "path": git_repo_path,
                });

                let response = client.post(request_url).json(&json_body).send()?;

                let git_repo_stats: lh_common::GitRepoStats = response.json()?;

                // Force colored output.
                colored::control::set_override(true);
                print!("{}", git_repo_stats.oneline());
            }
        }
        Some("path-shorten") => {
            if let Some(m) = matches.subcommand_matches("path-shorten") {
                // Safe to use unwrap() because of the required() option
                let path_to_shorten = m.value_of("path_to_shorten").unwrap();
                let path_aliases_file = m.value_of("path_aliases_file").unwrap();
                let path_aliases_contents =
                    fs::read_to_string(path_aliases_file).unwrap_or_default();

                let request_url = format!("{}/path-shorten", request_base_url);

                let json_body = json!({
                    "path": path_to_shorten,
                    "aliases": path_aliases_contents,
                });

                let response = client.post(request_url).json(&json_body).send()?;

                let path_shortened: PathShortened = response.json()?;

                print!("{}", path_shortened.path_shortened);
            }
        }
        Some("paths-sort") => {
            if let Some(m) = matches.subcommand_matches("paths-sort") {
                let paths_to_sort = m.value_of("paths_to_sort").unwrap();
                let priorities_raw_file = m.value_of("priorities_raw_file").unwrap();
                let priorities_raw_contents =
                    fs::read_to_string(priorities_raw_file).unwrap_or_default();

                let request_url = format!("{}/paths-sort", request_base_url);

                let json_body = json!({
                    "paths": paths_to_sort,
                    "priorities_raw": priorities_raw_contents,
                    "home": home_dir,
                });

                let response = client.post(request_url).json(&json_body).send()?;

                let paths_sorted: PathsSorted = response.json()?;

                print!("{}", paths_sorted.paths_sorted);
            }
        }
        Some("ping") => {
            let request_url = format!("{}/ping", request_base_url);

            client.get(request_url).send()?;

            println!("OK");
        }
        Some("shutdown") => {
            let request_url = format!("{}/shutdown", request_base_url);

            let response = client.get(request_url).send()?;

            println!("{}", response.text()?);
        }
        _ => unreachable!("clap should ensure we don't get here"),
    }
    Ok(())
}
