# Default (personal) values. Use these as a baseline, and modify further in
# ~/.zprofile depending on the machine as necessary.

. $HOME/.zsh/lib.sh

# Space-delimited list of directories to use for setting org-agenda-files. All
# *.org files in these directories are included, recursively.
export L_ORG_AGENDA_DIRS="$HOME/lo"
# Space-delimited list of exclusion patterns to remove files from
# org-agenda-files.
export L_ORG_AGENDA_EXCLUDE_PATTERNS="
	$HOME/lo/archived/
	$HOME/lo/plan.org
	$HOME/lo/note.org
	$HOME/lo/note/chart.org
	$HOME/lo/note/snippets.org
"

__l_prepend_path ~/.nix-profile/bin

case "$(hostname)" in
	w0) export TERM=xterm-24bit  ;;
	*) export TERM=wezterm ;;
esac

# Let emacs and other applications know that our terminal supports 24-bit colors.
export COLORTERM=truecolor

export MOOL_DIR="$HOME/.mool"
