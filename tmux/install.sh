#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

clone_or_pull()
{
	local url
	local path

	path="${2}"

	if [[ -d "${path}" ]]; then
		pushd "${path}"
			git pull
		popd
		return
	fi

	url="${1}"
	git clone "${url}" "${path}"
}

main()
{
	for target; do
		case "${target}" in
		tpm)
			clone_or_pull \
				"https://github.com/tmux-plugins/tpm" \
				~/.tmux/plugins/tpm
		;;
		*)
			echo >&2 "$0: unknown target \`${target}'"
			return 1
		;;
		esac
	done
}

main "$@"
