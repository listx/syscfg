#!/usr/bin/env bash

set -euo pipefail

copy_prefer_tmux()
{
	# Prefer to copy into tmux if possible, because it can also (with the -w
	# flag) attempt to copy into the system clipboard.
	if [[ -n "${TMUX:-}" ]]; then
		tmux load-buffer -w -
	else
		copy_clipboard
	fi
}

copy_clipboard()
{
	case "$(uname)" in
	Linux) xsel --clipboard ;;
	Darwin) pbcopy ;;
	esac
}

main()
{
	if [[ "${1:-}" == "--base64" ]]; then
		base64 --decode | copy_prefer_tmux
	else
		copy_prefer_tmux
	fi
}

main "$@"
