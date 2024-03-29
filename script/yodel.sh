#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# Given a list of youtube links, download them all.

for link in $(cat "$1"); do
	# If the link is just a plain video ID, construct the link.
	if [[ ! "${link}" =~ ^https://.* ]]; then
		link="https://www.youtube.com/watch?v=${link}"
	fi

	f="$(youtube-dl --get-filename -o '%(title)s-%(id)s' "$link")"
	youtube-dl --extract-audio "$link"
	r128gain "${f}"*
done
