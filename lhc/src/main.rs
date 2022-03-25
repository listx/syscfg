use clap::{app_from_crate, App, AppSettings, Arg};
use reqwest::blocking::Client;
use serde::Deserialize;
use serde_json::json;

use std::collections::HashMap;
use std::env;
use std::fs;

mod settings;
use settings::Settings;

#[derive(Deserialize, Debug)]
struct PathShortened {
    path_shortened: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let settings = Settings::new().unwrap();

    let matches = app_from_crate!()
        .global_setting(AppSettings::PropagateVersion)
        .global_setting(AppSettings::UseLongFormatForHelpSubcommand)
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand(
            App::new("path-shorten")
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
        .subcommand(App::new("ping").about("Check lh server connectivity"))
        .subcommand(App::new("shutdown").about("Shut down lh server instance"))
        .get_matches();
    let client = Client::new();
    match matches.subcommand_name() {
        Some("path-shorten") => {
            if let Some(m) = matches.subcommand_matches("path-shorten") {
                // Safe to use unwrap() because of the required() option
                let path_to_shorten = m.value_of("path_to_shorten").unwrap();
                let path_aliases_file = m.value_of("path_aliases_file").unwrap();
                let path_aliases_contents =
                    fs::read_to_string(path_aliases_file).unwrap_or_default();

                let mut subs = HashMap::new();
                let home_dir = env::var("HOME").expect("$HOME not set");

                // This is used to expand the definitions in the path aliases file.
                subs.insert("$HOME", &home_dir);

                // This is used to shrink the /home/... directory to "~" if
                // there is no path aliases match.
                let tilde = "~".to_string();
                subs.insert(&home_dir, &tilde);

                let request_url = format!(
                    "http://{}:{}/path-shorten",
                    settings.server.domain, settings.server.port
                );

                let json_body = json!({
                    "name": path_to_shorten,
                    "aliases_raw": path_aliases_contents,
                    "substitutions": subs,
                });

                let response = client.post(request_url).json(&json_body).send()?;

                let path_shortened: PathShortened = response.json()?;

                print!("{}", path_shortened.path_shortened);
            }
        }
        Some("ping") => {
            let request_url = format!(
                "http://{}:{}/ping",
                settings.server.domain, settings.server.port
            );

            let response = client.get(request_url).send()?;

            println!("{}", response.text()?);
        }
        Some("shutdown") => {
            let request_url = format!(
                "http://{}:{}/shutdown",
                settings.server.domain, settings.server.port
            );

            let response = client.get(request_url).send()?;

            println!("{}", response.text()?);
        }
        None => println!("Nothing to do."),
        _ => println!("Nothing to do (unrecognized subcommand)."),
    }
    Ok(())
}
