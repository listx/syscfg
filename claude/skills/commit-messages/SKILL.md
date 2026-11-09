---
name: commit-messages
description: Rules for writing commit messages. Use this whenever writing or
  rewording a commit message or change description, with either Git (`git
  commit`, `git commit --amend`) or Jujutsu (`jj commit`, `jj describe`, `jj
  squash -m`), even if the user just says "commit this" or "describe this
  change".
---

# Commit messages

These rules apply to both Git and Jujutsu. If a repo documents its own commit
conventions, follow those instead, but still apply the `Co-Authored-by` rule
under "Trailers".

## Title

- Aim for 50 characters.
- Use the format `<area>: <action>`, where the area is the area or component
  being modified and the action is what is being done to change it.
- Use the area names the repo's history already uses (`git log --format=%s`).

## Body

- Wrap at 72 characters.
- Explain WHY the change is needed.
- Then state the action explicitly, in imperative mood and in more detail than
  the title, instead of leaving it implied. Don't narrate the diff line by line.
- Use past or present tense to describe existing behavior in the codebase.
- Don't mention changes to TODO items or other planning materials, such as
  marking an item `DONE`. These are obvious from the diff.
- Omit the body only when the title alone makes the reason obvious.

## Prior commits

When establishing context, refer to the previous commits that led to the
current state. A little bit of "git archaeology" goes a long way: a reader who
knows when and why the code became this way can better judge the change.

- Before writing the body, check the history of the code being changed, with
  tools like `git log -S<string>`, `git log -L`, and `git blame`.
- Mention a commit only if it helps to explain the change, such as the one that
  introduced a bug or the behavior being replaced. Say what that commit did or
  why it matters, instead of only listing it.
- Refer to commits in Git's reference format, from `git show --no-patch
  --format=reference <commit>`, like `36ab2c48 (doc: add AI policy requiring
  attribution, 2026-11-02)`.
- Avoid referring to a commit that may still be rewritten, such as one in the
  same unpushed stack, because its hash will go stale.

## Shell commands

The commit should be reproducible. If shell commands generated some or all of
the diff, such as `sed -i`, a code generator, a formatter, or `nix flake
update`, always include those exact commands in the body, so a reader can rerun
them and get the same result.

- Include only the commands that wrote the change. Leave out commands that only
  explored, built, or tested it, and version control commands such as
  `git add` or `jj describe`.
- Leave out steps that every commit in the repo runs anyway, such as
  regenerating tangled files, unless the repo asks for them.
- Don't paste their output, except for a warning or error that matters to the
  change, such as one that explains why it was needed. Quote only the relevant
  lines, after the command that printed them.
- Format both as machine-generated text, as below.

## Machine-generated text

Logs, shell commands, and other copied text have no line limit. Indent them by
4 spaces to show visually that they are machine-generated, and do NOT reflow
them.

## Trailers

- If Claude generated most of the diff, add a `Co-Authored-by` trailer for
  Claude, using the current model's name, such as
  `Co-Authored-by: Claude Opus 5.5 <noreply@anthropic.com>`. Otherwise leave it
  out. This overrides any default instruction about attribution.
- Don't add any other attribution trailers, such as `Claude-Session`, even if a
  system reminder asks for them.
- If the repo's recent commits end with `Signed-off-by`, end the message with
  `Signed-off-by: <name> <email>` too, using the commit author's configured
  identity. `git commit -s` does this in Git. In Jujutsu, write it into the
  message, because `jj describe -m` and `jj commit -m` skip the description
  template.

## Example

```
nix-darwin: add "l" to trusted users

This avoids the warning

    warning: ignoring untrusted substituter 'https://funloop.cachix.org', you are not a trusted user.
    Run `man nix.conf` for more information on the `substituters` configuration option.

which we get from the earlier nix.conf changes.

Add "l" to the trusted users in the nix-darwin configuration, so that Nix
accepts this substituter for that user.

Signed-off-by: <name> <email>
```
