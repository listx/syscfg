#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

offlineimap -a main & pid1=$!

wait $pid1
echo "Synced at $(date)"

if [[ -n ${1:-} ]]; then
    sleep $1
fi
