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
	local input
	input=$(</dev/stdin)
	if [[ "${1:-}" == "--base64" ]]; then
		input="$(echo -n "${input}" | base64 --decode)"
	fi
	echo -n "${input}" | copy_prefer_tmux
	echo "${input}"
}

main "$@"
