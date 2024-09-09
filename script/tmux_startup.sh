#!/usr/bin/env bash

# Usage: $0
#
# Start up tmux panes and splits from scratch.

set -euo pipefail

is_blank_session()
{
	local session_name
	session_name="$(tmux display-message -p "#{session_name}")"
	tmux list-windows -t "${session_name}" -F "#{window_index}" | tail -n1 | grep -q 0
	tmux list-panes -t "${session_name}" -F "#{pane_index}" | tail -n1 | grep -q 0
}

main()
{
	local window_id

	window_id="$(tmux new-window -c ~/lo -P)"
	tmux send-keys -t "${window_id}" ./note/open.sh Space "${window_id}" Enter
	tmux new-window -c ~/prog/git
	tmux new-window -c ~/syscfg
	tmux previous-window
	tmux previous-window
}

main "$@"
