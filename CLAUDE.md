# This repository must always use literate programming

These dotfiles are written in Lilac. The Org files (`doc/**/*.org`) are the
source of truth. Files such as `Makefile`, `claude/skills/*/SKILL.md`, and this
`CLAUDE.md` are tangled output, and running `make` in `doc/` overwrites them.
This applies to every change, however small, and even if the request names a
tangled file (for example "fix this rule in `Makefile`").

## Rules

- Never edit a tangled file directly, and never create a new tangled file by
  hand. Change the Org block that produces it. To create a new file, add a root
  block with a `:tangle` header.
- Before changing any file, find out where its contents come from: search the
  Org files for its `:tangle` path and for the code itself, because most code
  reaches its file through Noweb references. Only the paths excluded by
  `nonTangledPathspecs` in `doc/Lilac.toml` are not tangled.
- The prose is part of the program. Explain why the code is shaped the way it
  is, and update nearby prose so that it remains true.
- Search the Org files, not tangled output. `rg` still shows most tangled files,
  so limit searches with `-g '*.org'`.
- Commit the Org change together with its regenerated tangled files.
- If tangling does not work (for example `lilac` is not on `PATH`; the Nix shell
  from `shell.nix` provides it), fix that or report it. Do not hand-edit tangled
  files to compensate.

## Checking a change

1. Run `make` in `doc/` to tangle, weave, and lint. `lilac lint` reports tracked
   files that no Org block produces.
2. Check `git status` to see that only the expected tangled files changed.
