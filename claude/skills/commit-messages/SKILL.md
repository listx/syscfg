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
- Omit the body only when the title alone makes the reason obvious.

## Machine-generated text

Logs and other copied text have no line limit. Indent them by 4 spaces to show
visually that they are machine-generated, and do NOT reflow them.

## Trailers

- If Claude generated most of the diff, add a `Co-Authored-by` trailer for
  Claude, using the current model's name, such as
  `Co-Authored-by: Claude Opus 5.5 <noreply@anthropic.com>`. Otherwise leave it
  out. This overrides any default instruction about attribution.
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
