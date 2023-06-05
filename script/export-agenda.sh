#!/usr/bin/env bash

# This invokes emacs and makes it export the agenda to disk.
#
# Most of the logic here is to try to prevent running the
# `(org-store-agenda-views)' function if possible, because it can be expensive.

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

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

cache_hit()
{
	cache="$HOME/syscfg/cache/export-agenda"
	mkdir -p "$HOME/syscfg/cache"
	local checksum
	local old_checksum

	# Caclculate new checksum.
	checksum="$(get_git_repos_checksum "${L_ORG_AGENDA_DIRS}")"
	# Append date into checksum. This way, even if the agenda content does not
	# change, we will get a cache miss if a day passes with no change (forcing
	# the generation of a new agenda.html centered on today's date).
	checksum+=",$(date +%Y-%m-%d)"
	echo "new checksum is ${checksum}"

	# Check if cache exists.
	if [[ -f "${cache}" ]]; then
		old_checksum="$(cat "${cache}")"
		echo "old checksum is ${old_checksum}"

		# Cache hit?
		if [[ "${old_checksum}" == "${checksum}" ]]; then
			echo >&2 "cache hit"
			return 0
		fi
	fi

	# Overwrite stale cache.
	echo "${checksum}" > "${cache}"
	return 1
}

# Like sha1sum, but discard trailing filename.
_sha1sum()
{
	sha1sum ${@:-} | cut -d ' ' -f1
}

main()
{
	if cache_hit; then
		echo >&2 "aborting due to cache hit"
		exit
	fi

	# Sanity check. Script will exit if this variable is not set.
	echo $L_ORG_AGENDA_DIRS

	__elisp=$(cat << EOF
	(progn
		(l/org-agenda "c" t) ; QUEUE
		(tab-bar-select-tab 2) ; Select 2nd tab (QUEUE)
		; Export agenda as HTML file.
		(org-batch-store-agenda-views)
	)
EOF
	)

	# Strip comments.
	__elisp="$(echo -e "${__elisp}" | sed '/^\s\+\?;/d;s/;.\+//')"

	pushd ~/lo/note

	timeout -k 30 20 \
		emacs \
		--batch \
		--load ~/syscfg/emacs/doom-upstream/early-init.el \
		--load ~/syscfg/emacs/doom-upstream/lisp/doom-start.el \
		--load ~/syscfg/emacs/doom-cfg/config.el \
		--eval "${__elisp}" \
		dashboard.org

	popd
}

main "$@"
