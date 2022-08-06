#!/usr/bin/env bash

# Usage: $0 <SESSION_ID> <WINDOW_INDEX> <L|R>
#
# Move the current tmux pane in the given session either left or right, across
# windows --- cycling to the front or back as necessary.

set -euo pipefail

__session_id="${1}"
__window_index="${2}"
__direction="${3}"

main()
{

	local window_index_last
	# This only works because we turn on "renumber-windows" in tmux.
	window_index_last="$(tmux list-windows -F \#I | tail -n1)"

	if [[ "${__direction}" == LEFT ]]; then
		# If we're already at the leftmost window, move the pane to the last
		# window.
		if (( "${__window_index}" == 0 )); then
			tmux join-pane -t "${__session_id}:${window_index_last}"
		else
			tmux join-pane -t "${__session_id}:$((__window_index - 1))"
		fi
	else
		# If we're at the rightmost window, then we have to move the pane to the
		# first window.
		if (( "${__window_index}" == "${window_index_last}" )); then
			tmux join-pane -t "${__session_id}:0"
		else
			tmux join-pane -t "${__session_id}:$((__window_index + 1))"
		fi
	fi

	tmux set key-table root
}

main "$@"
