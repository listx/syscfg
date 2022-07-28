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

        let head_sha = &self.head_sha;

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
