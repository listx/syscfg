#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

main()
{
	local host
	host="$(hostname)"

	# If there's a shorter name available, use it.
	if [[ -f ~/.hostname-short ]]; then
		# Only read the first 5 bytes. It better be short!
		host=$(head -c 5 ~/.hostname-short)
	fi

	# Replace non-tmux-session-name-friendly characters with an underscore.
	host="${host//./_}"

	if [[ -z "${1:-}" ]]; then
		echo "tmux sessions:"
		tmux list-sessions || true

		tmuxinator list --newline

		return
	fi

	local session="${1#$host-}"
	local host_session
	host_session="${host}-${session}"

	# Create a new raw tmux session. Attach to an existing session
	# of the same name if it is already running.
	tmux new-session -A -s "${host_session}"
}

main "$@"
