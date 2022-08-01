#!/usr/bin/env bash

set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

cd "${SCRIPT_ROOT}"

"${HOME}"/.nix-profile/bin/lorri daemon
