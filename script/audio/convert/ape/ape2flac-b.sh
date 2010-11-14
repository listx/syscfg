#!/bin/bash

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

# Count the number of APE files in this directory.
ape=`ls "$1" | grep -c \\.ape`

# If no APE files are found in this directory,
# then exit without error.
if [ $ape -lt 1 ]
then
	echo $1" (No APE files, moving on)"
	exit 0
else
	echo $1" ("$ape" APE files)"
fi

echo "Converting APE files to FLAC files."
shnconv -o flac "$1"/*.ape

echo ""
