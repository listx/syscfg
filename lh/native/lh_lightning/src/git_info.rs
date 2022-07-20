use git2::Repository;
use lh_common::GitInfo;
use std::error::Error;

#[rustler::nif]
pub fn git_info(path: &str) -> String {
    match git_info_raw(path) {
        Ok(gi) => serde_json::to_string(&gi).unwrap(),
        Err(e) => {
            println!("git_info: {:?}", e);
            "{}".to_string()
        }
    }
}

pub fn git_info_raw(path: &str) -> Result<GitInfo, Box<dyn Error>> {
    // Read Git repository information at given path. If there is no repo at the
    // given path, search upwards.
    let mut mrepo = Repository::discover(path)?;

    // Perform mutable borrow.
    let stashed_count = get_stashed_count(&mut mrepo);

    // Now from this point on, use the immutable reference.
    let repo = &mrepo;

    let empty = repo.is_empty()?;
    if empty {
        return Err("empty repo".into());
    }

    let head = repo.head()?;
    let sha = get_sha(&head)?;
    let branch = get_branch(&head);

    if repo.is_bare() {
        let mut ret = GitInfo::default();
        ret.bare = true;
        ret.head_sha = sha;
        ret.head_branch = branch;
        return Ok(ret);
    }

    let (ahead, behind) = get_upstream_divergence(&repo, &head)?;
    let (unstaged_hunks, staged_hunks) = get_hunks(&repo, &head)?;
    let untracked_count = get_untracked_count(&repo)?;
    let assume_unchanged_count = get_assume_unchanged_count(&repo)?;

    let ret = GitInfo {
        bare: false,
        head_sha: sha,
        head_branch: branch,
        head_ahead: ahead,
        head_behind: behind,
        hunk_changed: unstaged_hunks,
        hunk_staged: staged_hunks,
        untracked: untracked_count,
        stashed: stashed_count,
        assume_unchanged: assume_unchanged_count,
    };

    Ok(ret)
}

pub fn get_sha(gref: &git2::Reference) -> Result<String, Box<dyn Error>> {
    let commit = gref.peel_to_commit()?;
    let sha = commit.id().to_string()[0..7].to_string();

    Ok(sha)
}

pub fn get_branch(gref: &git2::Reference) -> String {
    // Use "HEAD" as branch name for detached HEAD mode.
    gref.shorthand().unwrap_or("HEAD").to_string()
}

// Calculate how much HEAD is ahead/behind its upstream branch. This is
// basically an equivalent of:
//
//      git rev-list --left-right --count HEAD...@{upstream}
pub fn get_upstream_divergence(
    repo: &git2::Repository,
    head: &git2::Reference,
) -> Result<(u32, u32), Box<dyn Error>> {
    let head_commit = head.peel_to_commit()?;
    let head_id = head_commit.id();
    let upstream_obj = repo.revparse_single("@{upstream}")?;
    let ancestor = repo.merge_base(head_id, upstream_obj.id())?;
    let ahead = count_commits(repo, ancestor, head_id)?;
    let behind = count_commits(repo, ancestor, upstream_obj.id())?;

    Ok((ahead, behind))
}

// Count all commits from a to b, excluding a but including b.
pub fn count_commits(
    repo: &git2::Repository,
    a: git2::Oid,
    b: git2::Oid,
) -> Result<u32, Box<dyn Error>> {
    let mut revwalk = repo.revwalk()?;

    // Start walking (backwards) from b.
    revwalk.push(b)?;

    // While traversing, if we have multiple parents, only follow the first
    // parent.
    revwalk.simplify_first_parent()?;

    // Exclude a. That is, stop traversal when we hit a.
    revwalk.hide(a)?;

    let mut count = 0;
    for _id in revwalk {
        count += 1;
    }

    Ok(count)
}

pub fn get_hunks(
    repo: &git2::Repository,
    head: &git2::Reference,
) -> Result<(u32, u32), Box<dyn Error>> {
    let head_tree = head.peel_to_tree()?;

    let diff = repo.diff_index_to_workdir(None, None)?;
    let diff_count = diff.deltas().count() as u32;

    let diff_cached = repo.diff_tree_to_index(Some(&head_tree), None, None)?;
    let diff_cached_count = diff_cached.deltas().count() as u32;

    Ok((diff_count, diff_cached_count))
}

pub fn get_untracked_count(repo: &git2::Repository) -> Result<u32, Box<dyn Error>> {
    let mut opts = git2::StatusOptions::new();
    opts.include_untracked(true).recurse_untracked_dirs(true);
    let statuses = repo.statuses(Some(&mut opts))?;

    let count = statuses
        .iter()
        .filter(|e| e.status() == git2::Status::WT_NEW)
        .count() as u32;

    Ok(count)
}

pub fn get_stashed_count(repo: &mut git2::Repository) -> u32 {
    let mut count = 0;
    let _ = repo.stash_foreach(|_, _, _| {
        count += 1;
        true
    });

    count
}

pub fn get_assume_unchanged_count(repo: &git2::Repository) -> Result<u32, Box<dyn Error>> {
    let index = repo.index()?;
    Ok(index
        .iter()
        .filter(|e| {
            let flags = git2::IndexEntryFlag::from_bits_truncate(e.flags);
            // is_valid() checks if the "assume valid" (i.e., --assume-unchanged
            // for git-update-index) flag is set. For a related discussion
            // around a (fixed) bug for the Ruby libgit2 bindings for this area,
            // see https://github.com/libgit2/rugged/issues/636.
            flags.is_valid()
        })
        .count() as u32)
}
