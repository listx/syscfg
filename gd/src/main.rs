use clap::Command;
use colored::*;
use std::process::Command as SCommand;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cmd = clap::Command::new("gd")
        .version("v0.0")
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(Command::new("diff").about("Show git unstaged and staged diffs."));
    let matches = cmd.get_matches();
    match matches.subcommand_name() {
        Some("diff") => {
            if let Some(_m) = matches.subcommand_matches("diff") {
                let diff = SCommand::new("git")
                    .args(["diff"])
                    .output()
                    .expect("git command failed to start");

                // We convert tabs into 4 spaces. This should ideally only
                // affect leading indentation, but we are too lazy to fix this
                // because 99.999% of the time literal tabs are only found in
                // the leading indentation anyway.
                let diff_output = String::from_utf8_lossy(&diff.stdout).replace("\t", "    ");

                // Split output lines, then for each line: (1) prepend the
                // vertical label char and (2) depending on leading +/- char
                // colorize the text fg and bg.

                for line in diff_output.lines() {
                    let mut words_iter = line.split_ascii_whitespace();

                    let cline = match words_iter.next() {
                        Some("diff") => line.bold().yellow(),
                        Some("index") => line.bold().yellow(),
                        Some("---") => line.bold().yellow(),
                        Some("+++") => line.bold().yellow(),

                        // This is not identical to git-diff (the latter does
                        // not colorize the entire line), but this is close
                        // enough.
                        Some("@@") => line.cyan(),

                        Some("+") => line.truecolor(0, 255, 0).on_truecolor(51, 85, 51),
                        Some("-") => line.truecolor(255, 0, 0).on_truecolor(85, 51, 51),

                        Some(_) => line.normal(),
                        None => "".normal(),
                    };
                    println!("{}", cline);
                }
            }
        }
        _ => unreachable!("clap should ensure we don't get here"),
    }
    Ok(())
}
