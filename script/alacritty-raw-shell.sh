#!/usr/bin/env bash

# Usage: $0
#
# This script starts an alacritty instance, but (1) clears out existing TMUX env
# vars, and (2) invokes the alacritty configuration for a raw shell (to avoid
# triggering TMUX from our ~/.zshrc). The first part is necessary because
# otherwise the L_TMUX_PANE_PWD_* environment variables pollute our environment
# and our prompt generation, not to mention that things like FZF_TMUX=1 will
# make running C-r invoke FZF on the existing TMUX server instance (possibly in
# a separate window on another workspace, yuck).
#
# The whole point of this script is to allow us to spawn a new, clean TMUX-less
# alacritty instance on Mac (on Linux our bindings are more flexible, obviating
# the need for this script), so that we can then SSH into another remote
# instance (where TMUX will be invoked again remotely).

set -euo pipefail

main()
{
	# Clear environment variables (if any) for TMUX* or L_TMUX*.
	while read -r env_var; do
		unset "${env_var%=*}"
	done < <(env | grep '^\(L_\)\?TMUX')

	2>/dev/null L_WANT_RAW_SHELL=1 alacritty --working-directory "${PWD}"
}

main "$@"
