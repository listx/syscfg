#!/bin/zsh
# Program: fiv (File Integrity Verifier --- uses sha512 checksum to verify file integrity
# Author: Linus Arver
# Date: 2010
# Usage: fiv [OPTIONS]

# OPTIONS

# -c FILE(S)  verify all sums found in FILE(S)
# (NO OPTION) create sums for all files found in current directory and dumpt it into STDOUT

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
Usage: fiv [OPTIONS] FILES

OPTIONS:

FILES           Generate sums of FILES and print them to STDOUT. If a directory
                  is given, recursively find every regular file under it and
                  generate sums for those files as well.
                You can also feed fiv output from another program with a pipe,
                  like so: find -type f | fiv
-c FILE         Read all sums found in FILE and verify them.
-h              Show this page and exit (regardless of other parameters).
-v              Show version number and exit (regardless of other parameters).

EXAMPLES:

  fiv *
    This gets all regular files from current directory, recursively, and prints
    their checksums to STDOUT.

  find -type f | fiv
    This gets all the results from the \`find -type f' command and feeds it to
    fiv. Fiv will then generate the checksums for each file.

  fiv * > checksums
    Same as above, but redirects the output into a (new) file called
    \`checksums'. The file should be a new file, because if it already exists,
    fiv will add the sums for that file as well.
"
            exit 0
            ;;
        "version")
            echo "fiv version 1.0"
            exit 0
            ;;
        *)
            echo "fiv: $1"
            exit 1
            ;;
    esac
}

# process STDIN, if any (timeout after 0.1 second)
stdinFiles=""
while read -s -t 0.1 stdinText; do
    stdinFiles="$stdinFiles '$stdinText'"
done

# if there was STDIN, process it (convert them to $@ arguments); otherwise, just display help msg
if [[ -n $stdinFiles ]]; then
    eval set -- "$@" $stdinFiles
else
    msg "help"
fi

files=()
cfile=""
recursive=0

# check for help and version arguments
while getopts "c:hv" opt; do
    case "$opt" in
    h)  msg "help" ;;
    v)  msg "version" ;;
    *) ;;
    esac
done

# re-parse from beginning
OPTIND=1
while getopts ":c:hv" opt; do
    case "$opt" in
    c)
        cfile="$OPTARG"
        ;;
    h)  msg "help" ;;
    v)  msg "version" ;;
    *)  exit 1
        ;;
    esac
done

if [[ -n $cfile ]]; then
    echo "${c3}fiv: Verifying sums from file \`$cfile'$ce"
    echo
    eval sha1sum -c $cfile 2>&1 | sed "s/^/  $c1>$ce /"
    exit_status=$pipestatus[1]
    if [[ $exit_status -eq 0 ]]; then
        echo "\n${c1}fiv: Everything OK$ce"
    else
        echo "\n${c6}fiv: sha1sum exited with error status $exit_status$ce"
    fi
else
    # Populate files.
    for thing in $@; do
        if [[ -f "$thing" ]]; then
            files+=("$thing")
        elif [[ -d "$thing" ]]; then
            find "$thing" -type f | while read line; do
                files+=("$line")
            done
        fi
    done
    # Generate sums
    for f in $files; do
        sha1sum $f
    done
fi

# vim: syntax=zsh
