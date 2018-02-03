#!/bin/zsh
# takes a list of files from FILE and copies them to the given folder

# colors
c1="\x1b[1;32m" # bright green
c2="\x1b[1;33m" # bright yellow
c3="\x1b[1;34m" # bright blue
c4="\x1b[1;36m" # bright cyan
c5="\x1b[1;35m" # bright purple
c6="\x1b[1;31m" # bright red
ce="\x1b[0m"

msg () {
	case $1 in
		"help")
			cat <<EOF
Usage: copy_plist_files FILE FOLDER

EXAMPLES:
	Moves all files in playlist into ~/music.
		$ copy_plist_files playlist ~/music
EOF
			exit 0
		;;
		"version")
			echo "copy_plist_files version 1.0"
			exit 0
		;;
		*)
			echo "copy_plist_files: $1"
			exit 1
		;;
	esac
}

# check for help and version arguments
while getopts "hv" opt; do
	case "$opt" in
	h)  msg "help" ;;
	v)  msg "version" ;;
	*)  ;;
	esac
done

# re-parse from beginning
OPTIND=1
while getopts "hv" opt; do
	case "$opt" in
	h)  msg "help" ;;
	v)  msg "version" ;;
	*)  exit 1
		;;
	esac
done

plist=$1
folder=$2

if [[ ! -e "$plist" ]]; then
	echo "File \`$plist' does not exist."
	exit 1
elif [[ ! -d "$folder" ]]; then
	echo "Folder \`$folder' does not exist."
	exit 1
else
	files=()
	while read l
	do
		if [[ $l[1] != '#' ]]; then
			files+=($l)
		fi
	done < $plist

	rsync -ahP --no-whole-file --inplace $files $folder
fi

# vim:syntax=zsh
