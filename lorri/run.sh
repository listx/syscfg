#!/usr/bin/env bash

set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

cd "${SCRIPT_ROOT}"

export PATH="${HOME}/.nix-profile/bin:/run/current-system/sw/bin:${PATH}"

lorri daemon
