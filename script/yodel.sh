#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# Given a list of youtube links, download them all.

for link in $(cat "$1"); do
	f="$(youtube-dl --get-filename -o '%(title)s-%(id)s.opus' "$link")"
	youtube-dl --extract-audio "$link"
	r128gain "${f}"
done
