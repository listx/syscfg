#!/usr/bin/env bash
#
# Given a Git commit SHA from either the command line or STDIN, get a link to
# the upstream web page for that commit. Currently only GitHub is supported.

set -euo pipefail

get_upstream_link()
{
	local sha="${1:-}"
	local remote

	# A remote will typically look like "git@github.com:foo/bar.git". We
	# have to clean this up to look like
	# "https://github.com/foo/bar/commit/<SHA>".
	remote="$(git rev-parse --abbrev-ref --symbolic-full-name "@{u}")"

	# Get rid of the "/master" if the remote is "origin/master", so we just
	# have "origin".
	remote="${remote%/*}"

	remote="$(git remote get-url "${remote}")"
	case "${remote}" in
	git@github.com*)
		if [[ "${remote}" =~ git@github.com: ]]; then
			remote="${remote#git@github.com:}"
			remote="${remote%.git}"
			remote="https://github.com/${remote}"
		fi
		;;
	*)
		echo >&2 "non-github upstreams are not supported"
		return 1
		;;
	esac

	echo "${remote}/commit/${sha}"
}

main()
{
	local sha="${1:-}"
	if [[ -z "${sha}" ]]; then
		sha=$(</dev/stdin)
	fi

	local link_target
	local link_desc

	link_target="$(get_upstream_link "${sha}")"
	link_desc="$(git show --no-patch --pretty=reference "${sha}")"

	echo "[[${link_target}][${link_desc}]]"
}

main "$@"
