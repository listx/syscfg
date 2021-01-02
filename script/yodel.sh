#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# Given a list of youtube links, download them all.

for link in $(cat "$1"); do
	youtube-dl -x "$link"
done
