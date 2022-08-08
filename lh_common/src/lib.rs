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
        // If the status is anything but finished or loading, early exit with that status.
        match self.status.as_str() {
            "FINISHED" | "LOADING" => (),
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
                "%B%F{cyan}C%f%b", unstaged_files, unstaged_insertions, unstaged_deletions,
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
        let mid_byte = &bytes[10];

        if mid_byte & 1 > 0 {
            Self::colorize_sha1(sha)
        } else {
            Self::colorize_sha2(sha)
        }
    }

    // Use a simple coloring scheme that uses a random number of color groups
    // and color orderings from a chosen set of 4 colors (RGBY).
    fn colorize_sha1(sha: &str) -> String {
        let bytes = Self::decode_hex(sha);

        let possible_color_groups: [[u8; 4]; 16] = [
            // Use the [2,2,2,2] pattern 3 times because we don't want to use [8] and [4,4]
            // (the latter especially so because colorize_sha2() also uses a [4,4] split).
            [2, 2, 2, 2],
            [2, 6, 0, 0],
            [3, 5, 0, 0],
            [2, 2, 2, 2],
            [5, 3, 0, 0],
            [6, 2, 0, 0],
            [2, 2, 4, 0],
            [2, 4, 2, 0],
            [4, 2, 2, 0],
            [2, 3, 3, 0],
            [3, 2, 3, 0],
            [3, 3, 2, 0],
            [2, 2, 2, 2],
            [1, 7, 0, 0],
            [7, 1, 0, 0],
            [5, 2, 1, 0],
        ];

        let possible_color_orders: [[u8; 4]; 32] = [
            [0, 1, 2, 3],
            [0, 1, 3, 2],
            [0, 2, 1, 3],
            [0, 2, 3, 1],
            [0, 3, 1, 2],
            [0, 3, 2, 1],
            [1, 0, 2, 3],
            [1, 0, 3, 2],
            [1, 2, 0, 3],
            [1, 2, 3, 0],
            [1, 3, 0, 2],
            [1, 3, 2, 0],
            [2, 0, 1, 3],
            [2, 0, 3, 1],
            [2, 1, 0, 3],
            [2, 1, 3, 0],
            [2, 3, 0, 1],
            [2, 3, 1, 0],
            [3, 0, 1, 2],
            [3, 0, 2, 1],
            [3, 1, 0, 2],
            [3, 1, 2, 0],
            [3, 2, 0, 1],
            [3, 2, 1, 0],
            [0, 1, 2, 3],
            [0, 1, 3, 2],
            [1, 0, 2, 3],
            [1, 0, 3, 2],
            [2, 3, 0, 1],
            [2, 3, 1, 0],
            [3, 2, 0, 1],
            [3, 2, 1, 0],
        ];

        let color_group_idx = bytes[19] & 0xf;
        let color_group_selection: [u8; 4] = possible_color_groups[color_group_idx as usize];

        let color_order_idx = bytes[18] & 0xf;
        let color_order_selection: [u8; 4] = possible_color_orders[color_order_idx as usize];

        let mut v = Vec::<&str>::new();
        v.push("%F{black}");

        let mut i: usize = 0;
        let mut j = 0;
        for group_size in color_group_selection {
            println!("");
            if i > 7 {
                break;
            }
            // Choose color for this group.
            match color_order_selection[j] {
                0 => v.push("%K{red}"),
                1 => v.push("%K{green}"),
                2 => v.push("%K{blue}"),
                _ => v.push("%K{yellow}"),
            }
            j = j + 1;

            if i == 0 {
                v.push(" ");
            }
            v.push(&sha[i..(i + group_size as usize)]);
            i = i + group_size as usize;
        }

        v.push(" %k%f");
        v.join("")
    }

    // Use 2 24-bit color backgrounds.
    fn colorize_sha2(sha: &str) -> String {
        let bytes = Self::decode_hex(sha);
        let bg1 = &bytes[17..20];
        let bg2 = &bytes[14..17];
        let fg1 = Self::white_or_black(bg1);
        let fg2 = Self::white_or_black(bg2);

        format!(
            "%K{{#{}}} %F{{{}}}{}%f%K{{#{}}}%F{{{}}}{}%f %k",
            sha[34..40].to_string(),
            fg1,
            sha[0..4].to_string(),
            sha[28..34].to_string(),
            fg2,
            sha[4..8].to_string()
        )
    }

    // https://stackoverflow.com/a/56678483/437583
    fn white_or_black(rgb: &[u8]) -> String {
        let r = rgb[0] as f32 / 255.0;
        let g = rgb[1] as f32 / 255.0;
        let b = rgb[2] as f32 / 255.0;

        let lum = Self::get_luminance(r, g, b);

        // lum_star is a value from 0 (black) to 100 (white) where 50 is the
        // __perceptual__ middle grey.
        let lum_star = Self::luminance_to_lum_star(lum);

        if lum_star > 50.0 {
            "black".to_string()
        } else {
            "white".to_string()
        }
    }

    fn get_luminance(r: f32, g: f32, b: f32) -> f32 {
        0.2126 * Self::srgb_to_lin(r)
            + 0.7152 * Self::srgb_to_lin(g)
            + 0.0722 * Self::srgb_to_lin(b)
    }

    fn srgb_to_lin(color_channel: f32) -> f32 {
        if color_channel <= 0.04045 {
            color_channel / 12.92
        } else {
            let n: f32 = (color_channel + 0.055) / 1.055;
            n.powf(2.4)
        }
    }

    fn luminance_to_lum_star(lum: f32) -> f32 {
        if lum <= (216.0 / 24389.0) {
            lum * (24389.0 / 27.0)
        } else {
            lum.powf(1.0 / 3.0) * 116.0 - 16.0
        }
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
