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
	$HOME/lo/note/chart.org
	$HOME/lo/note/snippets.org
"

export PATH=$HOME/.nix-profile/bin:$PATH

# Default path to look for the SDK installed via nixpkgs.
# Don't bother with Google Cloud SDK on Windows Subsystem for Linux.
if [[ -z "${WSL_DISTRO_NAME:-}" ]]; then
	export L_GOOGLE_CLOUD_SDK_PATH=$(dirname $(readlink $(which gcloud)))/../google-cloud-sdk
fi

case "$(hostname)" in
	w0) export TERM=xterm-24bit  ;;
	*) export TERM=alacritty-xtermlike ;;
esac

# Run a command, but wrap it around with set_theme() calls so as to run the
# command as a particular color theme.
run_with_theme() {
	local new_theme
	local old_theme="${TERM_COLOR_THEME:-PastelDark.dhall}"
	new_theme="${1}"
	shift
	if [[ "${old_theme}" != "${new_theme}" ]]; then
		set_theme "${new_theme}"
	fi
	"$@"
	set_theme "${old_theme}"
}
