use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default, Debug, PartialEq, Eq)]
pub struct GitRepoStats {
    pub status: String,
    pub root: String,
    pub bare: bool,
    pub head_sha: String,
    pub head_branch: String,
    pub head_ahead: u32,
    pub head_behind: u32,
    pub unstaged_files: u32,
    pub unstaged_insertions: u32,
    pub unstaged_deletions: u32,
    pub staged_files: u32,
    pub staged_insertions: u32,
    pub staged_deletions: u32,
    pub untracked_files: u32,
    pub stash_size: u32,
    pub assume_unchanged_files: u32,
    pub submodule_uninitialized: u32,
    pub submodule_out_of_sync: u32,
    pub submodule_merge_conflicts: u32,
}

impl GitRepoStats {
    pub fn oneline(&self) -> String {
        // If the status is anything but finished, early exit with that status.
        match self.status.as_str() {
            "FINISHED" => (),
            status => return status.to_string(),
        };

        let mut bare = "".to_string();
        if self.bare {
            bare = "<bare> ".to_string();
        }

        let head_sha = Self::colorize_sha(&self.head_sha);

        let mut unstaged_diffstat = "".to_string();
        let mut unstaged_files = "".to_string();
        let mut unstaged_insertions = "".to_string();
        let mut unstaged_deletions = "".to_string();
        if self.unstaged_files > 0 {
            unstaged_files = format!("\u{2022}{}", self.unstaged_files).to_string();
        }
        if self.unstaged_insertions > 0 {
            unstaged_insertions = format!("+{}", self.unstaged_insertions).to_string();
        }
        if self.unstaged_deletions > 0 {
            unstaged_deletions = format!("-{}", self.unstaged_deletions).to_string();
        }
        if self.unstaged_insertions > 0 || self.unstaged_deletions > 0 {
            unstaged_diffstat = format!(
                " {}{}{}{}",
                "%B%F{green}D%f%b", unstaged_files, unstaged_insertions, unstaged_deletions,
            );
        }

        let mut staged_diffstat = "".to_string();
        let mut staged_files = "".to_string();
        let mut staged_insertions = "".to_string();
        let mut staged_deletions = "".to_string();
        if self.staged_files > 0 {
            staged_files = format!("\u{2022}{}", self.staged_files).to_string();
        }
        if self.staged_insertions > 0 {
            staged_insertions = format!("+{}", self.staged_insertions).to_string();
        }
        if self.staged_deletions > 0 {
            staged_deletions = format!("-{}", self.staged_deletions).to_string();
        }
        if self.staged_insertions > 0 || self.staged_deletions > 0 {
            staged_diffstat = format!(
                " {}{}{}{}",
                "%B%F{magenta}S%f%b", staged_files, staged_insertions, staged_deletions,
            );
        }

        let mut untracked_files = "".to_string();
        if self.untracked_files > 0 {
            untracked_files = format!(
                " {}{}",
                "%B%F{yellow}N%f%b",
                self.untracked_files.to_string()
            );
        }

        let mut stash_size = "".to_string();
        if self.stash_size > 0 {
            stash_size = format!(" {}{}", "%B%F{196}T%f%b", self.stash_size.to_string());
        }

        let mut assume_unchanged_files = "".to_string();
        if self.assume_unchanged_files > 0 {
            assume_unchanged_files = format!(
                " {}{}",
                "%B%F{201}A%f%b",
                self.assume_unchanged_files.to_string()
            );
        }

        let mut submodule_diffstat = "".to_string();
        let mut submodule_uninitialized = "".to_string();
        let mut submodule_out_of_sync = "".to_string();
        let mut submodule_merge_conflicts = "".to_string();
        if self.submodule_uninitialized > 0 {
            submodule_uninitialized = format!("-{}", self.submodule_uninitialized).to_string();
        }
        if self.submodule_out_of_sync > 0 {
            submodule_out_of_sync = format!("+{}", self.submodule_out_of_sync).to_string();
        }
        if self.submodule_merge_conflicts > 0 {
            submodule_merge_conflicts = format!("U{}", self.submodule_merge_conflicts).to_string();
        }
        if self.submodule_uninitialized > 0
            || self.submodule_out_of_sync > 0
            || self.submodule_merge_conflicts > 0
        {
            submodule_diffstat = format!(
                " {}{}{}{}",
                "%B%F{blue}M%f%b",
                submodule_uninitialized,
                submodule_out_of_sync,
                submodule_merge_conflicts
            );
        }

        let head_branch = format!(" {}", self.head_branch);

        let mut head_ahead = "".to_string();
        if self.head_ahead > 0 {
            head_ahead = format!(
                " {}{}",
                "%B%F{green}\u{25b2}%f%b",
                self.head_ahead.to_string()
            );
        }

        let mut head_behind = "".to_string();
        if self.head_behind > 0 {
            head_behind = format!(
                " {}{}",
                "%B%F{red}\u{25bc}%f%b",
                self.head_behind.to_string()
            );
        }

        format!(
            "{}{}{}{}{}{}{}{}{}{}{}",
            bare,
            head_sha,
            unstaged_diffstat,
            staged_diffstat,
            untracked_files,
            stash_size,
            assume_unchanged_files,
            submodule_diffstat,
            head_branch,
            head_ahead,
            head_behind
        )
    }

    fn colorize_sha(sha: &str) -> String {
        let bytes = Self::decode_hex(sha);

        // Compute background mask from first byte (19 bytes left). Make the
        // mask be 10 bits long, surrounded by 1 bit on each side. We also force
        // the first 2 and last 2 bits be "on" for improved aesthetics.
        let mut bg_mask: u16 = (bytes[0] << 1).into();
        bg_mask = bg_mask | 0b1_1000_0001_1;

        // Compute contiguous groups of 1-bits in bg_mask; these locations are
        // colorized.
        let mut v = Vec::<&str>::new();
        let mut last_bit = false;
        let mut last_color: u8 = 0;
        for i in 0..10 {
            if ((1 << i) & bg_mask) > 0 {
                match last_bit {
                    // The previous bit was not turned on. Pick a new color.
                    false => {
                        // Pick color 0 through 7 (8 possibilities). For color 0
                        // use the underline formatting instead of black,
                        // because black is hard to see on a dark terminal.
                        //
                        // Offset by 1 byte to ignore the first byte used for
                        // bg_mask.
                        let color = &bytes[i + 1] & 0x7;
                        match color {
                            0 => v.push("%B%U%F{white}%K{black}"),
                            1 => v.push("%F{black}%K{red}"),
                            2 => v.push("%F{black}%K{green}"),
                            3 => v.push("%F{black}%K{yellow}"),
                            4 => v.push("%F{black}%K{blue}"),
                            5 => v.push("%F{black}%K{magenta}"),
                            6 => v.push("%F{black}%K{cyan}"),
                            _ => v.push("%F{black}%K{white}"),
                        }
                        last_color = color;
                    }
                    // Continue existing color.
                    true => {}
                };
                last_bit = true;
            } else {
                // If the last bit was on but the current bit is off, clear the
                // previous bit's color.
                if last_bit {
                    match last_color {
                        0 => v.push("%k%f%u%b"),
                        _ => v.push("%k%f"),
                    };
                };
                last_bit = false;
            }
            // Push character!
            match i {
                0 | 9 => v.push(&" "),
                _ => v.push(&sha[i - 1..i]),
            };
        }

        if last_bit {
            match last_color {
                0 => v.push("%k%f%u%b"),
                _ => v.push("%k%f"),
            };
        }

        v.join("")
    }

    // Adapted from https://stackoverflow.com/a/52992629/437583. Return the
    // parsed bytes or an empty vector if there was an error.
    pub fn decode_hex(s: &str) -> Vec<u8> {
        let parse = (0..s.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&s[i..i + 2], 16))
            .collect();
        match parse {
            Ok(vec) => vec,
            Err(e) => {
                eprintln!("{:?}", e);
                Vec::new()
            }
        }
    }
}

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct Prompt {
    pub path_short: String,
    pub git_repo_stats: GitRepoStats,
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
