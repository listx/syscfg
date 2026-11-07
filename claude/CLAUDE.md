# One Claude session per repo

Each repo has at most one Claude Code session that owns it. Other sessions send
the owner their change requests instead of editing the repo themselves.

## Session names

`ListAgents` shows session names but not working directories, so names are how
sessions find the owner of a repo.

- The session that owns a repo is named `<dir>-<suffix>`, where `<dir>` is the
  basename of the repo's directory and `<suffix>` is short and has no hyphens.
  For example, the owner of `~/syscfg` is named like `syscfg-d1`.
- To find a repo's owner, compare everything before the last hyphen of each
  session's name against the repo's directory name. `lilac-legacy-a2` owns
  `lilac-legacy`, not `lilac`.
- Claude Code names a new session this way by default (after its working
  directory, plus a short suffix). Keep that name. Don't rename the session
  after its task.
- If your session's name doesn't follow the convention, ask the user to rename
  it with `/rename <dir>-<suffix>` (alias `/name`). A new session can be started
  with a chosen name using `claude -n <name>`.

## Changing files in another repo

Before changing files in a repo other than your own working directory, run
`ListAgents`. If a session owns that repo, send it the request with
`SendMessage` and don't edit the files yourself. If no session owns it, ask the
user.

## Requests from other sessions

When you own a repo and get a request from another session, handle it under
this repo's own rules (CLAUDE.md, tests, and so on). Check with the user before
anything hard to undo, such as commits, pushes, deletions, or history rewrites.
