use clap::{crate_version, App, Arg};

use reqwest::blocking::Client;
use serde::Deserialize;
use serde_json::json;

#[derive(Deserialize, Debug)]
struct PathShortened {
    path_shortened: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Subcommands function exactly like sub-Apps, because that's exactly what they are. Each
    // instance of a Subcommand can have its own version, author(s), Args, and even its own
    // subcommands.
    //
    // # Help and Version
    // Just like Apps, each subcommand will get its own "help" and "version" flags automatically
    // generated. Also, like Apps, you can override "-V" or "-h" safely and still get "--help" and
    // "--version" auto generated.
    //
    // NOTE: If you specify a subcommand for your App, clap will also autogenerate a "help"
    // subcommand along with "-h" and "--help" (applies to sub-subcommands as well).
    //
    // Just like arg() and args(), subcommands can be specified one at a time via subcommand() or
    // multiple ones at once with a Vec<App> provided to subcommands().
    let matches = App::new("lhc")
        .version(crate_version!())
        .subcommand(
            App::new("path-shorten").about("Shorten a path").arg(
                Arg::new("path_to_shorten")
                    .about("the path to shorten")
                    .index(1)
                    .required(true),
            ),
        )
        .get_matches();

    match matches.subcommand_name() {
        Some("path-shorten") => {
            if let Some(m) = matches.subcommand_matches("path-shorten") {
                // Safe to use unwrap() because of the required() option
                let path_to_shorten = m.value_of("path_to_shorten").unwrap();

                let request_url = "http://localhost:8080/path-shorten/";

                let json_body = json!({
                "path": path_to_shorten,
                });

                let response = Client::new().post(request_url).json(&json_body).send()?;

                let path_shortened: PathShortened = response.json()?;

                print!("{}", path_shortened.path_shortened);
            }
        }
        None => println!("Nothing to do."),
        _ => println!("Nothing to do (unrecognized subcommand)."),
    }
    Ok(())
}
