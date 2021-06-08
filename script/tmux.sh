#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

tmuxinator_project_exists()
{
	tmuxinator list | grep -v '^tmuxinator projects:' | grep -q "^${1}"
}

main()
{
	local host
	host="$(hostname)"
	# Replace non-tmux-session-name-friendly characters with an underscore.
	host="${host//./_}"

	if [[ -z "${1:-}" ]]; then
		echo "tmux sessions:"
		tmux list-sessions || true

		tmuxinator list

		return
	fi

	local session="${1#$host-}"
	local host_session
	host_session="${host}-${session}"

	# Try to use a tmuxinator project if the desired tmux session name matches a
	# tmuxinator project name. If a tmux session named "${host_session}" is
	# already running, attach to it.
	if tmuxinator_project_exists "${session}"; then
		tmuxinator start \
			--suppress-tmux-version-warning=SUPPRESS-TMUX-VERSION-WARNING \
			--name="${host_session}" \
			"${session}"
	else
		# If the desired tmux session name is not a tmuxinator project name,
		# then just create a new raw tmux session. Attach to an existing session
		# of the same name if it is already running.
		tmux new-session -A -s "${host_session}"
	fi
}

main "$@"
