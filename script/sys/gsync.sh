#!/usr/bin/env bash

# Sync git repositories with GNU Parallel.

set -o errexit
set -o nounset
set -o pipefail

com="$@"

repos=(
	k/notes_work_imvu
	secure
	syscfg
)

parallel --no-notice -k "echo -en '{}:\n  ' && git -C {} $com 2>&1 && echo" ::: ${repos[@]}
