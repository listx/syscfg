#!/usr/bin/env bash

set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

cd "${SCRIPT_ROOT}"

# Daemonize with the "-d" flag.
if [[ "${1:-}" == -d ]]; then
    nix-shell --command "nohup mix run --no-halt >/dev/null & disown"
else
    nix-shell --command "mix run --no-halt"
fi
