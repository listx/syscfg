use clap::{Arg, ArgAction, Command};
use colored::*;
use std::borrow::Cow;
use std::process::Command as SCommand;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cmd = clap::Command::new("gd")
        .version("v0.0")
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            Command::new("diff")
                .about("Show git unstaged and staged diffs.")
                .arg(
                    Arg::new("width")
                        .long("width")
                        .value_name("NUM")
                        .default_value("80")
                        .help("With of the terminal screen; used for coloring in missing trailing whitespace for better legibility. Default 80.")
                        .takes_value(true)
                        .required(false),
                )
                .arg(
                    Arg::new("staged")
                        .long("staged")
                        .help("Whether to get staged diff.")
                        .action(ArgAction::SetTrue),
                ),
        );
    let matches = cmd.get_matches();
    match matches.subcommand_name() {
        Some("diff") => {
            if let Some(m) = matches.subcommand_matches("diff") {
                let width = m
                    .get_one::<String>("width")
                    .unwrap()
                    .parse::<usize>()
                    .unwrap();
                let staged = m.get_one::<bool>("staged").unwrap();
                let diff = if *staged {
                    SCommand::new("git")
                        .args(["diff", "--staged"])
                        .output()
                        .expect("git command failed to start")
                } else {
                    SCommand::new("git")
                        .args(["diff"])
                        .output()
                        .expect("git command failed to start")
                };

                colored::control::set_override(true);
                show_diff(
                    &diff.stdout,
                    &width,
                    if *staged {
                        " STAGED ---------------------- "
                    } else {
                        " CHANGED --------------------- "
                    },
                    *staged,
                );
            }
        }
        _ => unreachable!("clap should ensure we don't get here"),
    }
    Ok(())
}

fn show_diff(output: &Vec<u8>, width: &usize, vlabel: &str, staged: bool) -> () {
    // We convert tabs into 4 spaces. This should ideally only
    // affect leading indentation, but we are too lazy to fix this
    // because 99.999% of the time literal tabs are only found in
    // the leading indentation anyway.
    let diff_output = String::from_utf8_lossy(output).replace("\t", "    ");

    // Split output lines, then for each line: (1) prepend the
    // vertical label char and (2) depending on leading +/- char
    // colorize the text fg and bg.

    let divider = " ".on_truecolor(0, 0, 0);

    let mut i = 0;
    for line in diff_output.lines() {
        let mut words_iter = line.split_ascii_whitespace();
        let (eline, escaped) = escape(line);

        let cline = if line.len() > 0 {
            if line.chars().nth(0).unwrap().is_ascii_whitespace() {
                format!(" {}", eline).normal()
            } else {
                match words_iter.next() {
                    Some("diff") => format!(" {}", eline)
                        .bold()
                        .truecolor(255, 255, 0)
                        .on_truecolor(85, 85, 51),
                    Some("index") => format!(" {}", eline)
                        .bold()
                        .truecolor(255, 255, 0)
                        .on_truecolor(85, 85, 51),
                    Some("---") => format!(" {}", eline)
                        .bold()
                        .truecolor(255, 255, 0)
                        .on_truecolor(85, 85, 51),
                    Some("+++") => format!(" {}", eline)
                        .bold()
                        .truecolor(255, 255, 0)
                        .on_truecolor(85, 85, 51),

                    // This is not identical to git-diff (the latter does
                    // not colorize the entire line), but this is close
                    // enough.
                    Some("@@") => format!(" {}", eline).cyan(),

                    // It could be that the word looks like "+foo" if "foo" is at the
                    // beginning of the eline. In this case we have to check for the
                    // first letter.
                    Some(w) => match &w[0..1] {
                        "+" => format!(" {}", eline)
                            .truecolor(0, 255, 0)
                            .on_truecolor(51, 85, 51)
                            .bold(),
                        "-" => format!(" {}", eline)
                            .truecolor(255, 0, 0)
                            .on_truecolor(85, 51, 51)
                            .bold(),
                        _ => format!(" {}", eline).normal(),
                    },

                    None => format!(" {}", eline).normal(),
                }
            }
        } else {
            "".normal()
        };

        // vlabel_part is surrounded with 3 spaces, for a total of 4 chars. We
        // also have to bump the width because each escaped character fools
        // println!() into thinking that the string is 1 char larger than it
        // really is.
        let rwidth = width - 4 + escaped;
        let vlabel_part = match &vlabel[i..i + 1] {
            " " => {
                if staged {
                    "   ".truecolor(255, 0, 255).on_truecolor(81, 51, 81)
                } else {
                    "   ".truecolor(0, 255, 255).on_truecolor(51, 81, 81)
                }
            }
            "-" => {
                if staged {
                    " \u{2503} ".truecolor(255, 0, 255).on_truecolor(81, 51, 81)
                } else {
                    " \u{2503} ".truecolor(0, 255, 255).on_truecolor(51, 81, 81)
                }
            }
            c => {
                if staged {
                    format!(" {} ", c)
                        .truecolor(255, 0, 255)
                        .on_truecolor(81, 51, 81)
                } else {
                    format!(" {} ", c)
                        .truecolor(0, 255, 255)
                        .on_truecolor(51, 81, 81)
                }
            }
        };
        println!("{}{}{:rwidth$}", vlabel_part, divider, cline);
        i = (i + 1) % vlabel.len();
    }
}

// Escapes a string to be fed into println!(). E.g., "foo\bar" becomes
// "foo\\bar".
//
// Adapted from
// https://github.com/njaard/sonnerie/blob/e6b82eed3d5ba53c2e667172985df9a967056b5d/escape_string/src/lib.rs#L166.
fn escape<'a>(text: &'a str) -> (Cow<'a, str>, usize) {
    let bytes = text.as_bytes();

    let mut owned = None;
    let mut escaped = 0;

    for pos in 0..bytes.len() {
        let special = match bytes[pos] {
            b'%' => Some(b'%'),
            b'\\' => Some(b'\\'),
            _ => None,
        };
        if let Some(s) = special {
            if owned.is_none() {
                owned = Some(bytes[0..pos].to_owned());
            }
            owned.as_mut().unwrap().push(s);
            owned.as_mut().unwrap().push(s);
            escaped = escaped + 1;
        } else if let Some(owned) = owned.as_mut() {
            owned.push(bytes[pos]);
        }
    }

    if let Some(owned) = owned {
        (
            unsafe { Cow::Owned(String::from_utf8_unchecked(owned)) },
            escaped,
        )
    } else {
        (
            unsafe { Cow::Borrowed(std::str::from_utf8_unchecked(bytes)) },
            escaped,
        )
    }
}
