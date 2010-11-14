#!/bin/bash

# Shortcut to the command to tag MP3 audio files in a
# directory tree with Replay Gain values. (The album gain value
# is set the same for all of the MP3 files in each directory.)
#
# This script takes as its argument the directory path
# (relative or absolute) to the head of a directory tree which
# contains MP3 files grouped into albums (each album in its
# own directory). The script tag-mp3-with-rg.sh is called for
# each directory found within this directory tree.
# The slash at the end of the directory path is not necessary
# (as it is added later in the tag-mp3-with-rg.sh script) but
# it doesn't seem to hurt if it is left on.
#
# E.g. `./tfwrg.sh "/media/music/mp3/Orange Goblin"`
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
	echo -e "\033[1;37;44m Arg "$1" is NOT a directory!\033[0m"
	exit $ARGUMENT_NOT_DIRECTORY
fi

echo -e "\033[1;37m********************************************************\033[0m"
echo -e "\033[1;37mCalling tag-mp3-with-rg.sh on each directory in:\033[0m"
echo -e "\033[1;36m"$1"\033[0m"
echo ""
find "$1" -type d -exec ~/syscfg/scripts/audio/replaygain/mp3/tag-mp3-with-rg.sh '{}' \;
