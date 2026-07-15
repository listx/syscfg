#!/usr/bin/env bash
# Usage: $0
#
# Start up various pre-defined tmux windows.

set -euo pipefail

# Check if the given tmux session is blank (has only 1 window and 1 pane). We
# check for equality to 0 because all windows and panes are 0-indexed. Use like
# this:
#
#     session_name="$(tmux display-message -p "#{session_name}")"
#     is_blank_session "${session_name}"
#
is_blank_session()
{
	local session_name="${1:-}"
	tmux list-windows -t "${session_name}" -F "#{window_index}" \
		| tail -n1 | grep -q 0
	tmux list-panes -t "${session_name}" -F "#{pane_index}" \
		| tail -n1 | grep -q 0
}

# Create windows for personal notes.
setup_notes()
{
	local window_emacs_notes

	window_emacs_notes="$(tmux new-window -c ~/lo -P)"
	tmux send-keys -t "${window_emacs_notes}" "./note/open.sh" Enter

}

setup_lilac()
{
	local window_emacs_lilac
	local window_jj_lilac
	local maybe_sleep

	window_emacs_lilac="$(tmux new-window -c ~/prog/lilac -P)"
	maybe_sleep="pgrep -f 'emacs --bg-daemon' >/dev/null || sleep 20"
	tmux send-keys -t "${window_emacs_lilac}" \
		"${maybe_sleep}; d base/lilac.org" Enter

	window_jj_lilac="$(tmux new-window -c ~/prog/lilac -P)"
	tmux send-keys -t "${window_jj_lilac}" "jl" Enter
}

init_session_1()
{
	# Start up main window with Emacs for personal notes.
	setup_notes

	# Create window for Emacs opened up at Lilac, with another extra blank
	# window for jj.
	setup_lilac

	# Go back to main window.
	tmux previous-window
	tmux previous-window
}

init_session_2()
{
	local current_window
	local window_fe_server

	# Navigate to Lilac folder, and start up tangle loop and frontend
	# server. Use the current window for the tangle loop script.
	current_window="$(tmux display-message \
		-p '#{session_name}:#{window_index}')"
	tmux send-keys -t "${current_window}" \
		"cd ~/prog/lilac && ./tangle-loop.sh" Enter

	# Start up frontend server.
	window_fe_server="$(tmux new-window -c ~/prog/lilac -P)"
	tmux send-keys -t "${window_fe_server}" "make dev-cljs" Enter

	# Show tangle loop window.
	tmux previous-window
}

main()
{
	case "${1:-1}" in
	1) init_session_1 ;;
	2) init_session_2 ;;
	*)
		echo >&2 "Unrecognized session ${1}"
		return 1
		;;
	esac
}

main "$@"
