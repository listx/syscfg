#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

while true; do
    offlineimap -o -a main & pid1=$!

    wait $pid1
    echo
    echo "EMAIL-SYNC: Synced at $(date)"

    if [[ -n ${1:-} ]]; then
        echo
        echo "Sleeping for $1 until next sync."
        sleep $1
    fi
done
