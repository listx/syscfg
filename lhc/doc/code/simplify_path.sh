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

set -euo pipefail

# Source utility functions.
SCRIPT_ROOT="$(dirname "$(readlink -f "$0")")"
. "${SCRIPT_ROOT}"/libzsh.sh

path="${1:-}"

is_named_path=0

__l_get_named_paths

# See if we hit any of the specially-named paths. If so, then we set final_path
# to include it.
for named_path in "${__l_named_paths[@]}"; do
	if [[ "${path}" != "${named_path#*=}"* ]]; then
		continue
	fi

	# Grab the path alias name. E.g., "s" for ~/syscfg.
	path_aliased="~${named_path%=*}"

	# Determine how long the full expanded path is. E.g., for ~/syscfg it may be
	# 14 characters for "/home/l/syscfg".
	path_expanded_len="${named_path#*=}"
	path_expanded_len="${#path_expanded_len}"

	# For the "prefix" that can be truncated, make it start from the portion of
	# the path that excludes the leading prefix that lines up with the expanded
	# path. E.g., if path is set to "/home/l/syscfg/foo/bar", then we want the
	# "prefix" to be just "foo/bar".
	prefix="${path:${path_expanded_len}}"
	# Remove leading '/', if any.
	prefix="${prefix#/}"

	# If the prefix has a slash in it (e.g., "foo/bar"), then we need to
	# identify the last portion "bar" as the basename, and any leading bit as
	# the prefix (which can be truncated).
	if [[ "${prefix}" == */* ]]; then
		basename="${prefix##*/}"
		prefix="${prefix%/*}"
		final_path="${path_aliased}/${prefix}/${basename}"
	else
		# This is a case where we're given just 1 child directory underneath the
		# expanded alias. E.g., "/home/l/syscfg/foo". In this case there is
		# nothing to truncate (both the path alias and the basename should be
		# fully displayed), so the final_path lacks a "prefix" portion.
		basename="${prefix}"
		final_path="${path_aliased}/${basename}"
	fi
	is_named_path=1
	break
done

if ! (( is_named_path )); then
	# Convert expanded $HOME path to just "~".
	if [[ "${path}" == "$HOME"* ]]; then
		path="~${path#$HOME}"
	fi
	prefix="${path%/*}"
	basename="${path##*/}"
	final_path="${prefix}/${basename}"
fi

# Truncate long prefix paths.
__TRUNCATE_THRESHOLD=30

i=0
mapfile -td / fields < <(printf "%s\0" "${prefix}")
while (( "${#final_path}" > "${__TRUNCATE_THRESHOLD}" )) && (( $i < "${#fields[@]}" )); do
	# Truncate this portion of the prefix to its first letter.
	fields["${i}"]="${fields[${i}]::1}"
	prefix=$(IFS=/; printf '%s' "${fields[*]}")
	if (( is_named_path )); then
		final_path="${path_aliased}/${prefix}/${basename}"
	else
		final_path="${prefix}/${basename}"
	fi
	i=$(($i + 1))
done

# Handle edge cases.
case "${final_path}" in
# $HOME path as input.
"~/~")
	echo "~"
	;;
"/")
	echo "/"
	;;
*)
	echo "${final_path%/}"
	;;
esac
