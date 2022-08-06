#!/usr/bin/env bash

# Usage: $0 <SESSION_ID> <WINDOW_INDEX> <L|R>
#
# Move the current tmux pane in the given session either left or right, across
# windows --- cycling to the front or back as necessary.

set -euo pipefail

main()
{
	local session_name
	local window_index
	local direction

	local target_window_index
	local window_index_last

	session_name="${1}"
	window_index="${2}"
	direction="${3}"

	# This only works because we turn on "renumber-windows" in tmux.
	window_index_last="$(tmux list-windows -F \#I | tail -n1)"

	if [[ "${direction}" == LEFT ]]; then
		# If we're already at the leftmost window, move the pane to the last
		# window.
		if (( "${window_index}" == 0 )); then
			target_window_index="${window_index_last}"
		else
			target_window_index="$((window_index - 1))"
		fi
	else
		# If we're at the rightmost window, then we have to move the pane to the
		# first window.
		if (( "${window_index}" == "${window_index_last}" )); then
			target_window_index="0"
		else
			target_window_index="$((window_index + 1))"
		fi
	fi

	tmux join-pane -t "${session_name}:${target_window_index}"
	tmux set key-table root
}

main "$@"
