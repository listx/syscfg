use colored::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct GitInfo {
    pub bare: bool,
    pub head_sha: String,
    pub head_branch: String,
    pub head_ahead: u32,
    pub head_behind: u32,
    pub hunk_changed: u32,
    pub hunk_staged: u32,
    pub untracked: u32,
    pub stashed: u32,
    pub assume_unchanged: u32,
}

impl GitInfo {
    pub fn oneline(&self) -> String {
        //let y = format!("{}", &"haha".to_string().red());
        let mut bare = "".to_string();
        if self.bare {
            bare = "<bare> ".to_string();
        }

        let head_sha = &self.head_sha;

        let mut hunk_changed = "".to_string();
        if self.hunk_changed > 0 {
            hunk_changed = format!(" {}{}", "D".bold().green(), self.hunk_changed.to_string());
        }

        let mut hunk_staged = "".to_string();
        if self.hunk_staged > 0 {
            hunk_staged = format!(" {}{}", "S".bold().magenta(), self.hunk_staged.to_string());
        }

        let mut untracked = "".to_string();
        if self.untracked > 0 {
            untracked = format!(" {}{}", "N".bold().yellow(), self.untracked.to_string());
        }

        let mut stashed = "".to_string();
        if self.stashed > 0 {
            stashed = format!(
                " {}{}",
                "T".bold().truecolor(255, 0, 0),
                self.stashed.to_string()
            );
        }

        let mut assume_unchanged = "".to_string();
        if self.assume_unchanged > 0 {
            assume_unchanged = format!(
                " {}{}",
                "A".bold().truecolor(255, 0, 255),
                self.assume_unchanged.to_string()
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
            hunk_changed,
            hunk_staged,
            untracked,
            stashed,
            assume_unchanged,
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
