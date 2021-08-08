#!/usr/bin/env bash

# Useful utility functions for understanding Zsh-isms.

# Recognize specially-named paths.
__l_get_named_paths()
{
	local path_expanded
	local path_alias
	__l_named_paths_definition="$HOME/syscfg/zsh/path-aliases"
	__l_named_paths=()
	while IFS= read -r line; do
		# Skip empty lines.
		if [[ -z "${line}" ]]; then
			continue
		fi
		# Skip comment lines.
		if [[ "${line}" == \#* ]]; then
			continue
		fi

		path_expanded="${line#*=}"
		# Convert literal "$HOME" to actual value of "$HOME".
		if [[ "${path_expanded}" == "\$HOME"* ]]; then
			path_expanded="${path_expanded/\$HOME/$HOME}"
		fi
		path_alias="${line%=*}"
		path_alias="${path_alias#hash -d}"
		# Remove leading whitespace characters. See
		# https://stackoverflow.com/a/3352015/437583.
		path_alias="${path_alias#"${path_alias%%[![:space:]]*}"}"
		__l_named_paths+=("${path_alias}=${path_expanded}")
	done < "${__l_named_paths_definition}"
}

# Take a named path that starts with a tilde, such as "~s/..." and expand the
# "~s" to its equivalent.
__l_expand_named_path()
{
	__l_get_named_paths

	local path
	local path_aliased
	local path_aliased_len
	local rest
	path="${1}"

	if ! [[ "${path}" =~ ^(~[^/]+) ]]; then
		echo "${path}"
		return
	fi

	path_aliased="${BASH_REMATCH[1]}"
	path_aliased_len="${#path_aliased}"
	rest="${path:${path_aliased_len}}"

	# At this point BASH_REMATCH[1] is path_aliased (the name of a named
	# directory). We just need to see if it is (1) recognized, and if so (2)
	# expand it to its raw equivalent.
	for named_path in "${__l_named_paths[@]}"; do
		# The ":1" in "${path_aliased:1}" is to skip over the initial "~" tilde
		# character.
		if [[ "${path_aliased:1}" != "${named_path%=*}" ]]; then
			continue
		fi

		echo "${named_path#*=}${rest}"
		return
	done

	# This path is using an unrecognized named directory. Still, print it out
	# because we have nothing better to do.
	echo "${path}"
}
