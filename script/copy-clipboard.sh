#!/usr/bin/env bash

set -euo pipefail

copy_clipboard()
{
	case "$(uname)" in
	Linux) xsel --clipboard ;;
	Darwin) pbcopy ;;
	esac
}

main()
{
	# Prefer to copy into tmux if possible, because it can also (with the -w
	# flag) attempt to copy into the system clipboard.
	if [[ -n "${TMUX:-}" ]]; then
		tmux load-buffer -w -
	else
		copy_clipboard
	fi
}

main
