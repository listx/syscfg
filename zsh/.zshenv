# Default (personal) values. Use these as a baseline, and modify further in
# ~/.zprofile depending on the machine as necessary.

# Space-delimited list of directories to use for setting org-agenda-files. All
# *.org files in these directories are included, recursively.
export L_ORG_AGENDA_DIRS="$HOME/lo"
# Space-delimited list of exclusion patterns to remove files from
# org-agenda-files.
export L_ORG_AGENDA_EXCLUDE_PATTERNS="
	$HOME/lo/archived/
	$HOME/lo/plan.org
	$HOME/lo/note.org
"

export TERM=xterm-256color
