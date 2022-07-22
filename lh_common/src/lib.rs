use colored::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct GitInfo {
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
}

impl GitInfo {
    pub fn oneline(&self) -> String {
        //let y = format!("{}", &"haha".to_string().red());
        let mut bare = "".to_string();
        if self.bare {
            bare = "<bare> ".to_string();
        }

        let head_sha = &self.head_sha;

        let mut unstaged_diffstat = "".to_string();
        let mut unstaged_insertions = "".to_string();
        let mut unstaged_deletions = "".to_string();
        if self.unstaged_insertions > 0 {
            unstaged_insertions = format!("+{}", self.unstaged_insertions).to_string();
        }
        if self.unstaged_deletions > 0 {
            unstaged_deletions = format!("-{}", self.unstaged_deletions).to_string();
        }
        if self.unstaged_insertions > 0 || self.unstaged_deletions > 0 {
            unstaged_diffstat = format!(
                " {}{}{}",
                "D".bold().green(),
                unstaged_insertions.green(),
                unstaged_deletions.red(),
            );
        }

        let mut staged_diffstat = "".to_string();
        let mut staged_insertions = "".to_string();
        let mut staged_deletions = "".to_string();
        if self.staged_insertions > 0 {
            staged_insertions = format!("+{}", self.staged_insertions).to_string();
        }
        if self.staged_deletions > 0 {
            staged_deletions = format!("-{}", self.staged_deletions).to_string();
        }
        if self.staged_insertions > 0 || self.staged_deletions > 0 {
            staged_diffstat = format!(
                " {}{}{}",
                "S".bold().magenta(),
                // For some inexplicable reason, colorizing these bits here makes
                // the generated Zsh prompt line eat the previous line. It's hard to
                // tell if it's the fault of the "colored" crate, Zsh, or Alacritty.
                //
                // It's probably due to the mixing of the ANSI escape sequences
                // here and the processing of the "%F", "%B", etc. tokens by
                // Zsh. So once we are able to unset PROMPT_SUBST, we should be
                // able to use colors here.
                staged_insertions,
                staged_deletions,
            );
        }

        let mut untracked_files = "".to_string();
        if self.untracked_files > 0 {
            untracked_files = format!(
                " {}{}",
                "N".bold().yellow(),
                self.untracked_files.to_string()
            );
        }

        let mut stash_size = "".to_string();
        if self.stash_size > 0 {
            stash_size = format!(
                " {}{}",
                "T".bold().truecolor(255, 0, 0),
                self.stash_size.to_string()
            );
        }

        let mut assume_unchanged_files = "".to_string();
        if self.assume_unchanged_files > 0 {
            assume_unchanged_files = format!(
                " {}{}",
                "A".bold().truecolor(255, 0, 255),
                self.assume_unchanged_files.to_string()
            );
        }

        let head_branch = format!(" {}", self.head_branch);

        let mut head_ahead = "".to_string();
        if self.head_ahead > 0 {
            head_ahead = format!(
                " {}{}",
                "\u{25b2}".bold().green(),
                self.head_ahead.to_string()
            );
        }

        let mut head_behind = "".to_string();
        if self.head_behind > 0 {
            head_behind = format!(
                " {}{}",
                "\u{25bc}".bold().red(),
                self.head_behind.to_string()
            );
        }

        format!(
            "[{}{}{}{}{}{}{}{}{}{}]",
            bare,
            head_sha,
            unstaged_diffstat,
            staged_diffstat,
            untracked_files,
            stash_size,
            assume_unchanged_files,
            head_branch,
            head_ahead,
            head_behind
        )
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
