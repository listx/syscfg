#!/bin/bash

# Shortcut to the command to tag FLAC audio files in a
# directory tree with Replay Gain values. (The album gain value
# is set the same for all of the FLAC files in each directory.)
#
# This script takes as its argument the directory path
# (relative or absolute) to the head of a directory tree which
# contains FLAC files grouped into albums (each album in its
# own directory). The script tag-flac-with-rg.sh is called for
# each directory found within this directory tree.
# The slash at the end of the directory path is not necessary
# (as it is added later in the tag-flac-with-rg.sh script) but
# it doesn't seem to hurt if it is left on.
#
# E.g. `./tfwrg.sh "/media/music/flac/Orange Goblin"`
#
# See www.bobulous.org.uk/misc/Replay-Gain-in-Linux.html
# for usage guide.
#


# Define error codes
ARGUMENT_NOT_DIRECTORY=10
FILE_NOT_FOUND=11

# Check that the argument passed to this script is a directory.
# If it's not, then exit with an error code.
if [ ! -d "$1" ]
then
	echo "Arg "$1" is NOT a directory!"
	exit $ARGUMENT_NOT_DIRECTORY
fi

echo "********************************************************"
echo "Calling tag-flac-with-rg.sh on each directory in:"
echo $1
echo ""
find "$1" -type d -exec /home/listdata/syscfg/shellscripts/replaygain/flac/tag-flac-with-rg.sh '{}' \;
