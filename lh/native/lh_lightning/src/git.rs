use lh_common::GitRepoStats;
use std::error::Error;
use std::path::Path;

#[rustler::nif]
pub fn is_git_index_file(path: &str) -> bool {
    path.contains(".git/") && path.ends_with("/index.lock")
}

#[rustler::nif]
pub fn is_git_repo(path: &str) -> bool {
    match git2::Repository::discover(path) {
        Ok(_) => true,
        _ => false,
    }
}

#[rustler::nif]
pub fn repo_stats(path: &str) -> String {
    match repo_stats_maybe(path) {
        Ok(gi) => serde_json::to_string(&gi).unwrap(),
        Err(e) => {
            println!("repo_stats_maybe: {:?}", e);
            "{}".to_string()
        }
    }
}

fn repo_stats_maybe(path: &str) -> Result<GitRepoStats, Box<dyn Error>> {
    // Read Git repository information at given path. If there is no repo at the
    // given path, search upwards.
    let mut repo = git2::Repository::discover(path)?;

    let empty = repo.is_empty()?;
    if empty {
        return Err("empty repo".into());
    }

    let (sha, branch) = get_sha_branch(&repo)?;

    let mut ret = GitRepoStats::default();

    ret.root = repo
        .workdir()
        .unwrap_or(Path::new(""))
        .to_str()
        .unwrap_or_default()
        .to_string();
    if repo.is_bare() {
        ret.bare = true;
        ret.head_sha = sha;
        ret.head_branch = branch;
        return Ok(ret);
    }

    let (ahead, behind) = if branch == "HEAD" {
        (0, 0)
    } else {
        match get_upstream_divergence(&repo) {
            Ok((ahead, behind)) => (ahead, behind),
            Err(e) => {
                println!("get_upstream_divergence: {:?}", e);
                (0, 0)
            }
        }
    };
    let assume_unchanged_count = get_assume_unchanged_count(&repo)?;
    let stashed_count = get_stashed_count(&mut repo);

    ret.bare = false;
    ret.head_sha = sha;
    ret.head_branch = branch;
    ret.head_ahead = ahead;
    ret.head_behind = behind;
    ret.stash_size = stashed_count;
    ret.assume_unchanged_files = assume_unchanged_count;

    Ok(ret)
}

fn get_sha_branch(repo: &git2::Repository) -> Result<(String, String), Box<dyn Error>> {
    let head = repo.head()?;
    let sha = get_sha(&head)?;
    let branch = get_branch(&head);
    Ok((sha, branch))
}

fn get_sha(gref: &git2::Reference) -> Result<String, Box<dyn Error>> {
    let commit = gref.peel_to_commit()?;
    let sha = commit.id().to_string()[0..7].to_string();

    Ok(sha)
}

fn get_branch(gref: &git2::Reference) -> String {
    // Use "HEAD" as branch name for detached HEAD mode.
    gref.shorthand().unwrap_or("HEAD").to_string()
}

// Calculate how much HEAD is ahead/behind its upstream branch. This is
// basically an equivalent of:
//
//      git rev-list --left-right --count HEAD...@{upstream}
fn get_upstream_divergence(repo: &git2::Repository) -> Result<(u32, u32), Box<dyn Error>> {
    let head = repo.head()?;
    let head_commit = head.peel_to_commit()?;
    let head_id = head_commit.id();
    let upstream_obj = repo.revparse_single("@{upstream}")?;
    let ancestor = repo.merge_base(head_id, upstream_obj.id())?;
    let ahead = count_commits(repo, ancestor, head_id)?;
    let behind = count_commits(repo, ancestor, upstream_obj.id())?;

    Ok((ahead, behind))
}

// Count all commits from a to b, excluding a but including b.
fn count_commits(
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

fn get_stashed_count(repo: &mut git2::Repository) -> u32 {
    let mut count = 0;
    let _ = repo.stash_foreach(|_, _, _| {
        count += 1;
        true
    });

    count
}

fn get_assume_unchanged_count(repo: &git2::Repository) -> Result<u32, Box<dyn Error>> {
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
