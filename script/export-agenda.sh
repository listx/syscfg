#!/usr/bin/env bash

# This invokes emacs and makes it export the agenda to disk. The details are in
# the `l/export-agenda' function in the emacs config.
#
# Most of the logic here is to try to prevent running the `l/export-agenda'
# function if possible, because it is expensive. It is expensive for 2 reasons:
# (1) the function loads color themes because it affects the HTML CSS styling of
# the exported file, and (2) we are forced to run a separate (new) emacs
# invocation because running against an existing daemon means blocking that
# daemon while this function runs (and also we don't want to have a dedicated
# separate emacs daemon just for this command only).

set -o errexit
set -o nounset
set -o pipefail

# Create a simple checksum of the git state inside a given git directory. We
# compute this by simply getting (1) current commit sha, plus any deltas found
# in (2) the work tree (git diff) or (3) the index (git diff -c cached), and
# also (4) any untracked files (git status).
get_git_repo_checksum()
{
	local git_repo_dir="${1}"
	local head_sha
	local files_shas
	pushd "${git_repo_dir}" >/dev/null
	head_sha="$(git rev-parse HEAD)"
	files_shas="$(git ls-files -z --exclude-standard --cached --deleted --modified --others | xargs --null sha1sum)"
	popd >/dev/null

	# Simplify the checksums to a single sha1sum.
	echo "${head_sha}${files_shas}" | _sha1sum
}

get_git_repos_checksum()
{
	local checksum=""
	for git_repo in "$@"; do
		echo >&2 "checksumming git repo \`${git_repo}'"
		checksum+="$(get_git_repo_checksum "${git_repo}")"
	done

	# Simplify the checksums to a single sha1sum.
	echo "${checksum}" | _sha1sum
}

exit_if_cache_hit()
{
	local cache="$HOME/syscfg/cache/export-agenda"
	mkdir -p "$HOME/syscfg/cache"
	local checksum
	local old_checksum

	# Caclculate new checksum.
	checksum="$(get_git_repos_checksum "${L_ORG_AGENDA_DIRS}")"
	echo "new checksum is ${checksum}"

	# Check if cache exists.
	if [[ -f "${cache}" ]]; then
		old_checksum="$(cat "${cache}")"
		echo "old checksum is ${old_checksum}"

		# Exit if the cached file has not changed.
		if [[ "${old_checksum}" == "${checksum}" ]]; then
			echo >&2 "cache has not changed; aborting"
			return 1
		fi
	fi

	# Overwrite old cache.
	echo "${checksum}" > "${cache}"
}

# Like sha1sum, but discard trailing filename.
_sha1sum()
{
	sha1sum ${@:-} | cut -d ' ' -f1
}

main()
{
	exit_if_cache_hit

	timeout -k 10 45 ~/.nix-profile/bin/emacs \
		--batch \
		--load ~/syscfg/emacs/init.el \
		--eval '(l/export-agenda)'
}

main "$@"
