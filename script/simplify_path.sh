#!/usr/bin/env bash

# Truncate a path, based on __TRUNCATE_THRESHOLD. E.g., this script converts
#
#   ~/aaaaaaaaaaaaaa/bbbbbbbbbbbbbbbbb/cccccccccccccc/hello
#
# into
#
#   ~/a/b/c/hello
#
# and is meant to be used for TMUX window titles, although it can be used
# elsewhere as well.

set -euxo pipefail

path="${1:-}"

if [[ "${path}" == "$HOME"* ]]; then
	path="~${path#$HOME}"
fi

prefix="${path%/*}"
basename="${path##*/}"

__TRUNCATE_THRESHOLD=30

# Truncate long prefix paths.
final_path="${prefix}/${basename}"
i=0
mapfile -td / fields < <(printf "%s\0" "${prefix}")
while (( "${#final_path}" > "${__TRUNCATE_THRESHOLD}" )) && (( $i < "${#fields[@]}" )); do
	# Truncate this portion of the prefix to its first letter.
	fields["${i}"]="${fields[${i}]::1}"
	prefix=$(IFS=/; printf '%s' "${fields[*]}")
	final_path="${prefix}/${basename}"
	i=$(($i + 1))
done

# Handle edge case for the $HOME path as input.
if [[ "${final_path}" == "~/~" ]]; then
	echo "~"
	exit 0
fi

echo "${final_path}"
