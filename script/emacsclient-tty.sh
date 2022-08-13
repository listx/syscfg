#!/usr/bin/env bash

set -euo pipefail

set_elisp()
{
	local file
	local buffer_filename
	local maybe_fill_72=""
	local maybe_goto_position=""
	local maybe_open_new_tab=""
	local position
	local line
	local column

	for arg; do
		if [[ "${arg}" == +* ]] && (($# > 1)); then
			position="${arg}"
		else
			file="${arg}"
		fi
		shift
	done

	case "${position:-}" in
	+*:*)
		line="${position#+}"
		line="${line%:*}"
		column="${position#*:}"
		maybe_goto_position="(evil-goto-line ${line}) (move-to-column ${column})"
		;;
	+*)
		line="${position#+}"
		maybe_goto_position="(evil-goto-line ${line})"
		;;
	esac

	buffer_filename="$(readlink -e "${file}")"

	# If Git is invoking this editor (for `git commit'), then manually set the
	# line length to 72.
	case "${buffer_filename}" in
	*COMMIT_EDITMSG | *git-rebase-todo)
		maybe_fill_72="(setq fill-column 72)"
		;;
	esac

	# Exit if we can't determine the $buffer_filename, because it is required.
	if [[ -z "${buffer_filename}" || ! -e "${buffer_filename}" ]]; then
		echo >&2 "could not determine \$buffer_filename; aborting"
		exit 1
	fi

	# Protect filename by "quoting" it, as per
	# https://www.gnu.org/software/emacs/manual/html_node/emacs/Quoted-File-Names.html,
	# but only if we need to. This usually only comes up rarely if we need to
	# look at filenames with such characters in them.
	#
	# In most circumstances, we don't want to bother doing this because it does
	# not play well with various third-party Emacs packages that expect the
	# filename to not have a leading "/:", such as linters and formatters for
	# programming languages.
	if [[ "${buffer_filename}" =~ [*:~!?] ]]; then
		buffer_filename="/:${buffer_filename}"
	fi

	# If we're just trying to tell the daemon to open the file without
	# allocating a tty (from inside an emacs vterm session), then create a new
	# tab first. This prevents "overriding" the current window (destroying
	# splits, etcs) from inside vterm which can be a bit jarring.
	if inside_emacs_vterm; then
		maybe_open_new_tab="(tab-new)"
	fi

	__elisp=$(
		cat <<EOF
	(prog1
		${maybe_open_new_tab}
		; Open the file.
		(find-file "${buffer_filename}")
		${maybe_fill_72}
		${maybe_goto_position}
		; Always show current line position, even if we're opening an
		; already-opened buffer.
		(hl-line-highlight-now)
		; Disable menu (for some reason the menu is enabled on the frame
		; sometimes).
		(menu-bar-mode 0)
		; Kill any keyboard prompts.
		(keyboard-escape-quit)
	)
EOF
	)

	# Strip comments.
	__elisp="$(echo -e "${__elisp}" | sed '/^\s\+\?;/d;s/;.\+//')"
}

# If we're invoking this script from within a vterm session already from inside
# emacs (presumably emacsclient), then just make emacsclient open the file
# without trying to allocate a tty.
open_file_from_vterm()
{
	# If we're inside an emacsclient vterm session already, then we just need to
	# tell the daemon to open the file.
	emacsclient --eval "${__elisp}"
	exit
}

inside_emacs_vterm()
{
	[[ -n "${EMACS_VTERM_PATH:-}" ]]
}

main()
{
	set_elisp "$@"

	if inside_emacs_vterm; then
		open_file_from_vterm
	fi

	# The (find-file ...) avoids showing "*scratch*" buffer on startup when
	# invoking from emacsclient.
	exec emacsclient \
		--alternate-editor "" \
		--tty \
		--eval "${__elisp}"
}

main "$@"
