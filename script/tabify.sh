#!/usr/bin/env zsh

setopt ERR_EXIT
setopt NO_UNSET
setopt PIPE_FAIL

# Convert leading spaces of shell scripts to a tab.

if [[ -z ${1:-} ]]; then
	cat <<EOF
Usage: ./tabify.sh EXT
	Need file extension EXT to search. EXT is the extension without the period.
Example:
	# Search for all files ending in "*.sh".
	./tabify.sh sh
EOF
	exit 1
fi

ext=$1

find . \
	-type f \
	-name \*.$ext \
	-exec sh \
	-c 'unexpand --first-only --tabs=4 {} > {}.fixed && mv {}.fixed {}' \;
