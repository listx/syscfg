#!/usr/bin/env zsh

# Push files to android device.
#
# Usage: apush.sh [-p destination_folder] <FILES...>
#
# NOTE: the "-p" flag MUST come first, because of our hacky usage of the "argc"
# variable to mark the beginning of "$@".

argc=0
OPTIND=1
while getopts ":p:" opt; do
	case "$opt" in
	p)
		dest_suffix="$OPTARG"
		((argc+=2))
		;;
	*)  exit 1
		;;
	esac
done

# Set default location.
dest="/sdcard/Download"
if [[ -n "$dest_suffix" ]]; then
	dest="${dest}/${dest_suffix}"
fi

shift $argc
echo $@

for f in $@; do
	adb push "$f" "$dest"
done
