#!/bin/bash

# convert all *.ape files found into .flac files

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
echo $1
echo ""
find "$1" -type d -exec /home/listdata/syscfg/script/audio/convert/ape/ape2flac-b.sh '{}' \;
