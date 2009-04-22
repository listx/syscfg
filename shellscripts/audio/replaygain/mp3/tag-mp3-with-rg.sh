#!/bin/bash

# tag-mp3-with-rg.sh
# Script created by Bobulous, October 2008.
# See www.bobulous.org.uk/misc/Replay-Gain-in-Linux.html
#
# This script takes as an argument a directory name,
# then uses mp3gain to add Replay Gain tags (for album and
# track gain) to each mp3 file in that directory.
# Then mp3gain is used to display the Replay Gain values for
# each track.
#
# Use find (with -exec) to call this script on
# a directory structure containing mp3 files, so that this
# script is run on each directory in that structure. E.g.
# find ./music/mp3 -type d -exec ~/tag-mp3-with-rg.sh '{}' \;
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
	echo -e "\033[1;37mArg "$1" is NOT a directory!\033[0m"
	exit $ARGUMENT_NOT_DIRECTORY
fi

# Count the number of mp3 files in this directory.
mp3num=`ls "$1" | grep -c \\.mp3`

# If no mp3 files are found in this directory,
# then exit without error.
if [ $mp3num -lt 1 ]
then
	echo -e "\033[1;33m"$1" \033[1;37m--> (No mp3 files found)\033[0m"
	exit 0
else
	echo -e "\033[1;36m"$1" \033[1;37m--> (\033[1;32m"$mp3num"\033[1;37m mp3 files)\033[0m"
fi

# Run mp3gain on the mp3 files in this directory.
echo -e ""
echo -e "\033[1;37mForcing (re)calculation of Replay Gain values for mp3 files and adding them as APE2 tags into the mp3 file...\033[0m"
echo -e ""
# first delete any APE replay gain tags in the files
mp3gain -s d "$1"/*.mp3
# add fresh APE tags back into the files
mp3gain "$1"/*.mp3
echo -e ""
echo -e "\033[1;37mDone.\033[0m"
echo -e ""
echo -e "\033[1;37mAdding ID3 tags with the same calculated info from above...\033[0m"
echo -e ""
# the -d is for debug messages if there are any errors, and the -f is for overwriting any existing ID3 replay gain tags
~/syscfg/shellscripts/audio/replaygain/mp3/ape2id3.py -df "$1"/*.mp3
echo -e ""
echo -e "\033[1;37mDone.\033[0m"
#-----------------------------------------------------------------------------------------------------#
## NOTE: This section is not needed, since the calls to mp3gain above by themselves generate output   #
## about replay gain information.                                                                     #
##                                                                                                    #
## Output the newly-created Replay Gain values for the mp3                                            #
## files in this directory.                                                                           #
#echo -e "\033[1;37mNewly-calculated Replay Gain (APE) values are:\033[0m"                            #
#mp3files=`ls -1 "$1"/*.mp3`                                                                          #
#IFS=$'\012' # separate file names correctly (use newlines)                                           #
#for file in $mp3files                                                                                #
#do                                                                                                   #
#        if [ ! -e "$file" ]                                                                          #
#        then                                                                                         #
#        # This should not happen -- unless these files get deleted or corrupt since the script began.#
#                echo -e "\033[1;31mError: file "$file" not found.\033[0m"                            #
#                exit $FILE_NOT_FOUND                                                                 #
#        fi                                                                                           #
#                                                                                                     #
#        echo -e "\033[1;32m"$file"\033[0m"                                                           #
#        mp3gain -s c "$file"                                                                         #
#        echo -e ""                                                                                   #
#done                                                                                                 #
#-----------------------------------------------------------------------------------------------------#

echo -e ""
echo -e "\033[1;37mReplay gain tags (both APE and ID3) successfully added recursively.\033[0m"
echo -e ""
