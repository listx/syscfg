#!/bin/bash

# tag-flac-with-rg.sh
# Script created by Bobulous, October 2008.
# See www.bobulous.org.uk/misc/Replay-Gain-in-Linux.html
#
# This script takes as an argument a directory name,
# then uses metaflac to add Replay Gain tags (for album and
# track gain) to each FLAC file in that directory.
# Then metaflac is used to display the Replay Gain values for
# each track.
#
# Use find (with -exec) to call this script on
# a directory structure containing FLAC files, so that this
# script is run on each directory in that structure. E.g.
# find ./music/flac -type d -exec ~/tag-flac-with-rg.sh '{}' \;
#
# See www.bobulous.org.uk/misc/Replay-Gain-in-Linux.html
#

# Error codes
ARGUMENT_NOT_DIRECTORY=10
FILE_NOT_FOUND=11

# Check that the argument passed to this script is a directory.
# If it's not, then exit with an error code.
if [ ! -d "$1" ]
then
	echo "Arg "$1" is NOT a directory!"
	exit $ARGUMENT_NOT_DIRECTORY
fi

# Count the number of FLAC files in this directory.
flacnum=`ls "$1" | grep -c \\.flac`

# If no FLAC files are found in this directory,
# then exit without error.
if [ $flacnum -lt 1 ]
then
	echo $1" (No FLAC files, moving on)"
	exit 0
else
	echo $1" ("$flacnum" FLAC files)"
fi

# Run metaflac on the FLAC files in this directory.
echo "Calculating Replay Gain values for FLAC files."
metaflac --add-replay-gain "$1"/*.flac

# Output the newly-created Replay Gain values for the FLAC
# files in this directory.
echo "Newly-calculated Replay Gain values:"
flacfiles=`ls -1 "$1"/*.flac`
IFS=$'\012' # separate file names correctly (use newlines)
for file in $flacfiles
do
	if [ ! -e "$file" ]
	then
		# This should not happen.
		echo "Error: file "$file" not found."
		exit $FILE_NOT_FOUND
	fi

	echo $file
	metaflac --show-tag=REPLAYGAIN_TRACK_GAIN "$file"
	metaflac --show-tag=REPLAYGAIN_ALBUM_GAIN "$file"
	echo ""
done

echo ""
