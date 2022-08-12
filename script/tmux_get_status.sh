#!/usr/bin/env bash

# Usage: $0 <SESSION_NAME> <PANE_MODE> <CLIENT_KEY_TABLE>
#
# Generate the status line for tmux.

set -euo pipefail

# Imitate emacs evil-mode's "<N>" and "<I>" modal signifiers to help
# distinguish between modes. When we're in "prefix mode", it is normal mode
# "<N>", where we can press any number of other keys without having to first
# press C-b (the prefix key). By default we start out in "insert" mode "<I>"
# which is when keys are passed through to the underlying application.
#
# "<V>" signifies copy-mode. We use "<V>" because copy-mode resembles Vim's
# visual selection mode.
#
# For other key tables, use the format "<key-table-name>" if L_TMUX_DEBUG is set
# to 1.
#
# Summary
#
# key-table or mode   | indicator
# --------------------+----------
# root                | <I>
# prefix              | <N>
# copy-mode/view-mode | <V>
# passthrough         | <P>
# other               | <other>
main()
{
	local session_name
	local pane_mode
	local client_key_table

	session_name="${1}"
	pane_mode="${2}"
	client_key_table="${3}"

	local session_format
	local style1

	if [[ "${pane_mode}" =~ (copy|view)-mode ]]; then
		style1=" #[bg=green fg=${L_TMUX_COLOR_TEXT}] <V> "
	elif [[ "${client_key_table}" == prefix ]]; then
		style1=" #[bg=blue fg=${L_TMUX_COLOR_TEXT}] <N> "
	elif [[ "${client_key_table}" == root ]]; then
		style1=" #[bg=${L_TMUX_COLOR_TEXT} fg=${L_TMUX_COLOR_CURSOR}] <I> "
	elif [[ "${client_key_table}" == passthrough ]]; then
		style1=" #[bg=cyan fg=${L_TMUX_COLOR_TEXT}] <P> "
	else
		# Display unrecognized key table names directly.
		style1=" #[bg=red fg=${L_TMUX_COLOR_TEXT}] <${client_key_table}> "
	fi

	session_format=" ${session_name} "
	# If we're SSH'ed into a nested tmux session, colorize the session name a
	# bit differently.
	if [[ -n "${SSH_CONNECTION:-}" ]]; then
		session_format="#[bg=blue] ${session_name} "
	fi

	echo "${session_format}#[bg=${L_TMUX_COLOR_TEXT}]${style1}#[bg=${L_TMUX_COLOR_TEXT}] "
}

main "$@"
