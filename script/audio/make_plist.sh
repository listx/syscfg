#!/usr/bin/env zsh
# creates a playlist for mplayer

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
echo "\
Usage: make_plist [OPTIONS]

OPTIONS:

-a              Search for all audio files recursively from the current
                directory. (Default: only search audio files from current
                directory).
-p FILENAME     Put generated playlist into FILENAME.
-h              Show this page and exit (regardless of other parameters).
-v              Show version number and exit (regardless of other parameters).

EXAMPLES:

  make_plist
    Find all audio files from current directory.

  make_plist -a
    Find all audio files from current directory, recursively.
"
            exit 0
            ;;
        "version")
            echo "make_plist version 1.0"
            exit 0
            ;;
        *)
            echo "make_plist: $1"
            exit 1
            ;;
    esac
}

search_flag="-maxdepth 1"
pname="plist"

# check for help and version arguments
while getopts "ap:hv" opt; do
    case "$opt" in
    h)  msg "help" ;;
    v)  msg "version" ;;
    *)  ;;
    esac
done

# re-parse from beginning
OPTIND=1
while getopts ":ap:hv" opt; do
    case "$opt" in
    a)
        search_flag=""
        ;;
    p)
        pname="$OPTARG"
        ;;
    h)  msg "help" ;;
    v)  msg "version" ;;
    *)  exit 1
        ;;
    esac
done

com="find $search_flag -type f -iregex \".+\\.\\(aac\\|flac\\|m4a\\|mp3\\|ogg\\|wav\\)\""

if [[ ! -e "$pname" ]]; then
    echo -n "Creating file \`$pname'..."
    file_list=$(eval $com | sort)
    if [[ -n $file_list ]]; then
        echo "$file_list" >> $pname
        echo "done."
        echo "Displaying file \`$pname':\n"
        cat "$pname" | sed "s/^/  $c1>$ce /"
        fnum=$(cat "$pname" | wc -l)
        echo "\nFound $fnum audio files (and listed in \`$pname')"
    else
        if [[ -z "$search_flag" ]]; then
            echo "\nNo audio files found (recursively)"
        else
            echo "\nNo audio files found (from current directory)"
            echo "  Use \`-a' option to search recursively."
        fi
    fi
else
    echo "File \`$pname' already exists."
    echo "Displaying dry run:"
    fnum=$(eval $com | wc -l)
    eval $com | sort | sed "s/^/  $c1>$ce /"
    echo "\nFound $fnum audio files (dry run)"
fi
