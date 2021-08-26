#!/usr/bin/env bash

# Usage: $0 <pane_current_path> <mouse_word>
#
# This script is meant to be invoked from tmux with the DoubleClick1Pane mouse
# action. It tries to make sense of the given <mouse_word> with respect to the
# <pane_current_path>, and tries to do its best to handle it in an intelligent
# way.
#
# Currently it does one of 4 things, based on what <mouse_word> is:
#
#   - (directory): cd into it
#   - (file): open it with emacsclient as a TMUX pane popup
#   - (file:NUM): open it with emacsclient, and go to line NUM as a TMUX popup
#   - (other): copy it as a Tmux "buffer" (aka "register" in Vim-lingo)
#
# Note that previewing files with a floating TMUX pane popup (with emacsclient)
# depends on the `pane_current_path' value to resolve the filename, and so will
# most likely not work if (1) we run emacsclient and (2) within emacsclient run
# vterm to list files and (3) double-click on files from there.

set -euo pipefail

__pane_id="${1}"
__pane_current_path="${2}"
__mouse_word="${3}"
__exit_copy_mode="${4:-}"

# "univ_open" is the function that "d" is aliased to in Zsh.
__l_univ_open()
{
	tmux send-keys -t "${__pane_id}" " d ${__mouse_word}" Enter
}

__l_complete_and_univ_open()
{
	tmux send-keys -t "${__pane_id}" " d ${__mouse_word}" Tab Enter
}

__l_edit()
{
	tmux display-popup -t "${__pane_id}" -w90% -h90% -E -E -d "${__pane_current_path}" " \
		~/syscfg/script/emacsclient-tty.sh \"${__mouse_word}\" \
	"
}

__l_edit_at_line()
{
	tmux display-popup -t "${__pane_id}" -w90% -h90% -E -E -d "${__pane_current_path}" " \
		~/syscfg/script/emacsclient-tty.sh \
			\"${__mouse_word%:*}\" \
			\"+${__mouse_word#*:}\" \
	"
}

__l_copy_to_tmux_buffer()
{
	local tmpfile
	tmpfile=$(mktemp)

	echo -n "${__mouse_word}" > "${tmpfile}"
	tmux load-buffer -t "${__pane_id}" "${tmpfile}"
	rm -f "${tmpfile}"
	tmux display-message -t "${__pane_id}" -d1000 "copied \"${__mouse_word}\""
}

main()
{
	# Exit copy-mode if we are already in it.
	if [[ -n "${__exit_copy_mode}" ]]; then
		tmux send-keys -t "${__pane_id}" q
	fi

	# If the word starts with '/' or '~', just try to cd into it. We virtually
	# press a TAB key to resolve any named directories or shortened directory
	# names (because Zsh can recognize paths like "/a/b/c" to mean
	# "/apple/banana/carrot").
	if [[ "${__mouse_word}" =~ ^[/~] ]]; then
		__l_complete_and_univ_open
	# If the word is interesting (it's a path that can be reached from PWD),
	# then either cd into it, or open it if it's not a child directory.
	elif [[ -d "${__pane_current_path}/${__mouse_word}" ]]; then
		# There's no need to quote #{mouse_word} here because if the user
		# clicks on a directory with spaces in its name, only part of the
		# directory name will get copied as "#{mouse_word}" anyway,
		# defeating the "[[ -d ... ]]" check above.
		__l_univ_open
	elif [[ -e "${__pane_current_path}/${__mouse_word}" ]]; then
		# This is most likely a regular text file, so just open it with
		# emacsclient.
		__l_edit
	# If the word doesn't exist, check if it's of the form "file:NUM", and if
	# "file" does exit, open it up at that line number.
	elif [[ "${__mouse_word}" =~ ^[^:]+:[0-9]+$ ]] \
		&& [[ -e "${__pane_current_path}/${__mouse_word%:*}" ]]; then
		__l_edit_at_line
	# If the word is not a recognizable file from PWD, copy it in. This was
	# derived from the default DoubleClick1Pane binding with vanilla Tmux.
	else
		__l_copy_to_tmux_buffer
	fi
}

main "$@"
