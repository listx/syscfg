#!/usr/bin/env bash

# Usage: $0 <PANE_CURRENT_COMMAND> <PANE_CURRENT_PATH> <WINDOW_NAME> <WINDOW_ID> <PANE_ID> <WINDOW_FLAGS> <WINDOW_PANES>
#
# Generate the window name and formatting/style/colors for all active and inactive windows.

set -euo pipefail

main()
{
	local pane_current_command
	local pane_current_path
	local pane_id
	local window_name
	local window_id
	local window_name_generated
	local window_flags
	local window_panes
	local is_current_window

	pane_current_command="${1}"
	pane_current_path="${2}"
	window_name="${3}"
	window_id="${4}"
	pane_id="${5}"
	window_flags="${6}"
	window_panes="${7}"
	is_current_window="${8}"

	local pane_count
	local style1
	local style2

	if [[ "${pane_current_command}" == zsh ]]; then
		window_name_generated="$(~/syscfg/script/tmux_pane_pwd_cached.sh "${window_id}" "${pane_id}" "${pane_current_path}")"
		style1="#[bg=black fg=yellow]"
		style2="#[bg=black fg=yellow]"
		if (( is_current_window )); then
			style1="#[bg=yellow]"
			style2="#[bg=brightyellow fg=black]"
		fi
	elif [[ "${window_name}" =~ ^ssh-\> ]]; then
		window_name_generated="${window_name}"
		style1="#[bg=black fg=blue]"
		style2="#[bg=black fg=blue]"
		if (( is_current_window )); then
			style1="#[bg=blue]"
			style2="#[bg=brightblue fg=black]"
		fi
	else
		window_name_generated="${pane_current_command}"
		style1="#[bg=black fg=green]"
		style2="#[bg=black fg=green]"
		if (( is_current_window )); then
			style1="#[bg=green]"
			style2="#[bg=brightgreen fg=black]"
		fi
	fi

	if (( "${window_panes}" > 1 )); then
		pane_count="$(( window_panes - 1 ))"
	else
		pane_count=" "
	fi

	echo "${style1}${window_flags:- }${pane_count}${style2} ${window_name_generated} #[default]"
}

main "$@"
