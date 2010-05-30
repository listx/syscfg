#!/bin/zsh
# Program: aex
# Version: 1.0
# Author: Linus Arver
# License: GPLv3

# Copyright (C) 2010 Linus Arver

# aex, for "advanced extractor," takes one or more archive files and extracts
# them all intelligently to prevent (1) so-called "tar bombs" that have twenty,
# thirty files in them in the archive's root directory, and (2) spurious,
# nested folders that simply contain other folders (i.e., if an archive,
# after extraction, looks like ./archive-name/archive-name/archive-name/files,
# or the like, aex will remove these middle-men folders to make it look like
# ./archive-name/files).
#
# The beauty of aex is that it can take multiple archive types (a mixture of, say, *.zip and *.tar.gz) and extract them all in one go.
#
# Below is a quick summary of how aex operates.
#
# 1. Verify that every single file passed is a recognized archive type, is a valid existing file, and ensure that a directory bearing the same basename(s) as the file(s) do not already exist
# 2. For each file, do the following:
#    a. Create a new directory based on the file's name
#    b. Extract the file into that directory
#    c. Remove the chain of nested directories, if any

# Colors (comment out to disable)
# format: "1;" means bright, and "38;5;{NUM}" means to use the color NUM from the 256-color palette
c1="\e[1;38;5;120m" # bright green
c2="\e[1;38;5;228m" # bright yellow
c3="\e[1;38;5;214m" # bright orange
c4="\e[1;38;5;159m" # bright cyan
c5="\e[1;38;5;175m" # bright purple
c6="\e[1;38;5;160m" # bright red
ce="\e[0m"

# color codes, but for 'sed' command only
c11="[1;38;5;120m" # bright green
c22="[1;38;5;228m" # bright yellow
c33="[1;38;5;214m" # bright orange
c44="[1;38;5;159m" # bright cyan
c55="[1;38;5;175m" # bright purple
c66="[1;38;5;160m" # bright red
cee="[0m"

# Error messages
aex_msg() {
    case $1 in
        0)
            echo "aex: operation aborted (no change)"
            exit 1
            ;;
        1)
            echo "aex: directory \`$2' was created by someone since this script was initially called"
            echo "aex: operation aborted"
            exit 1
            ;;
        2)
            echo "aex: extraction of file \`$c1$2$ce' failed (file format/extension mismatch, or damaged archive)"
            exit 1
            ;;
        8)
            echo "aex: extracting file \`$c1$2$ce' into current directory with \`"$c3$3$ce"'...\n"
            ;;
        9)
            echo "aex: extracting file \`$c1$2$ce' into \`$c2$3$ce' with \`"$c3$4$ce"'...\n"
            ;;
        99)
            echo "aex: unexpected fatal error while working with file \`$c1$2$ce'"
            echo "aex: operation aborted"
            exit 1
            ;;
        *) # this will only happen if we have a typo in our code when calling aex_msg()!
            echo "aex: unrecognized error"
            ;;
    esac
}

# A function to get the basename of an archive
aex_fb() {
    fb=""
    # get basename of archive file
    case $1 in
        *.tar.bz2)
            fb=$(basename $1 .tar.bz2)
            ;;
        *.tar.gz)
            fb=$(basename $1 .tar.gz)
            ;;
        *.tar.xz)
            fb=$(basename $1 .tar.xz)
            ;;
        *)
            # use zsh's built-in ':r' extension-remover
            fb=$1:r
            ;;
    esac
    echo $fb
}

#----------------#
# PROGRAM START! #
#----------------#

# various error checking before we do anything
for f in $@; do
    # ensure that we recognize all arguments' archive types
    case $f in
        *.tar|*.tar.bz2|*.tbz2|*.bz2|*.tar.gz|*.tgz|*.gz|*.tar.xz|*.txz|*.xz|*.zip|*.rar|*.7z)
            ;;
        *)
            echo "aex: $f: \`$f:e' is not a recognized archive file format"
            aex_msg 0
            ;;
    esac

    fb=$(aex_fb $f)

    # ensure that the given file(s) actually exist (as a regular file, not a
    # special file or directory)
    if [[ ! -f $f ]]; then
        echo "aex: \`$f' does not exist"
        aex_msg 0
    # ensure that a directory with the same basename does not already exist
    elif [[ -d $fb ]]; then
        echo "aex: destination directory \`$fb' already exists"
        aex_msg 0
    fi
done

fbs=() # array to be filled by basename directory names ($fb below)
dir0=$PWD

# begin extraction!
current=1
for f in $@; do
    fb=$(aex_fb $f)
    fbs+=($fb) # append $fb as an element into the $fbs array
    echo "\naex: ($current/$#): processing \`$c1$f$ce'..."

    # For the tar archives (since they are easy to work with uniformly), first
    # into the archive, and see if (1) the archive holds a SINGLE DIRECTORY
    # that contains all files and (2) this directory has the SAME NAME as the
    # archive's basename ($fb) -- if so, we can skip the creation of a
    # directory named ($fb) (thus avoiding the waste of detecting 1 single
    # "nested directory") for most archive types
    dir_create=true
    case $f in
        *.tar|*.tar.gz|*.tgz|*.tar.bz|*.tar.bz2|*.tbz|*.tbz2|*.tar.xz|*.txz)
            top=$(tar tf $f | sort | head -n 1 | sed 's/\///')
            if [[ $(tar tf $f | sed "s/^$top\//\//g" | sed 's/^\/.*//g' | sed '/^$/d' | wc -l) -eq 0 && $top == $fb ]]; then
                dir_create=false
                echo "aex: root directory in archive matches suggested destination directory name"
                echo "aex: skipping directory creation"
            fi
            ;;
    esac

    if $dir_create; then
        echo -n "aex: creating destination directory \`$c2$fb$ce'... "
        # raise error and exit if between the starting of this script, and
        # until it finishes, someone manually created a directory with the same
        # name that we are about to extract into (thus making the "mkdir $fb"
        # command fail)
        mkdir $fb 1>&- 2>&- || aex_msg 1 $fb
        # (FYI: the "1>&- 2>&-" syntax manually suppresses stdout and stderr, without using /dev/null)
        echo "done"
    fi

    case $f in
        *.tar|*.tar.gz|*.tgz|*.tar.bz|*.tar.bz2|*.tbz|*.tbz2|*.tar.xz|*.txz)
            # two -v's for more verbosity than just a single -v
            # extract the file with given info, but if the extracting program fails, then inform user of this (in case the program's own error messages aren't clear enough)
            # the '2>&1' makes stderr print to stdout (so that sed can catch it)
            if $dir_create; then
                aex_msg 9 $f $fb "tar xvf $f -C $fb"
                tar xvf $f -C $fb 2>&1 | sed "s/^/  $c11>$cee /" || aex_msg 2 $f
            else
                aex_msg 8 $f "tar xvf $f"
                tar xvf $f 2>&1 | sed "s/^/  $c11>$cee /" || aex_msg 2 $f
            fi
            ;;
        *.gz)
            cd $fb
            aex_msg 9 $f $fb "gzip -dv ../$f"
            gzip -dv ../$f 2>&1 | sed "s/^/  $c11>$cee /" || aex_msg 2 $f
            cd ..
            ;;
        *.bz|*.bz2)
            cd $fb
            aex_msg 9 $f $fb "bzip2 -dv ../$f"
            bzip2 -dv ../$f 2>&1 | sed "s/^/  $c11>$cee /" || aex_msg 2 $f
            cd ..
            ;;
        *.xz)
            cd $fb
            aex_msg 9 $f $fb "xz -dv ../$f"
            xz -dv ../$f 2>&1 | sed "s/^/  $c11>$cee /" || aex_msg 2 $f
            cd ..
            ;;
        *.zip)
            aex_msg 9 $f $fb "unzip $f -d $fb"
            unzip $f -d $fb 2>&1 | sed "s/^/  $c11>$cee /" || aex_msg 2 $f
            ;;
        *.rar)
            aex_msg 9 $f $fb "urar x $f $fb"
            unrar x $f $fb 2>&1 | sed "s/^/  $c11>$cee /" || aex_msg 2 $f
            ;;
        *.7z)
            aex_msg 9 $f $fb "7z x -o$fb $f"
            7z x -o$fb $f 2>&1 | sed "s/^/  $c11>$cee /" || aex_msg 2 $f
            ;;
        *)
            # NOTE: this should never happen (I've tried to make message 99 happen, by starting the extraction on a file, then renaming it before the extraction program is called, but still it does not reach this area!)
            aex_msg 99 $f # should never occur -- it's just in here for the sake of completeness
            ;;
    esac

    # remove nested directories (directories that only contain 1 child directory) in
    # the extracted-to directory above (we call such directories "eggs")
    cd $fb
    dir1=$PWD # $dir1 is same as $fb, but is nicer b/c it is the full directory path
    dir2=""

    eggs=0
    echo -n "\naex: nested directories detected inside \`$c2$fb$ce': "
    if [[ $(ls -A1 | wc -l) -eq 1 && -d $(ls -A) ]]; then
        echo -n "$c4$(ls -A)$ce "
        cd $(ls -A)
        while [[ $(ls -A1 | wc -l) -eq 1 && -d $(ls -A) ]]; do
            echo -n $c2"->"$ce $c4$(ls -A)$ce" "
            cd $(ls -A)
            [[ eggs -eq 0 ]] && dir2=$PWD # name the very first egg "dir2"
            let eggs=eggs+1
        done
        echo
    else
        echo "none"
    fi

    # if there were any eggs, move the children up to $fb's level
    if [[ $eggs -gt 0 ]]; then
        echo "aex: moving contents of non-nested directory \`$c4$(echo $PWD:t)$ce' to \`$c2$dir1:t$ce'\n"
        # the *(D) simply means the same as '*', but will also match dotfiles ('D' does this)
        mv -v *(D) $dir1 2>&1 | sed "s/^/  $c44>$cee /"

        # move back to root of destination directory, to remove the lineage of eggs (everything
        # below $dir2 contains a single lineage of nested directories -- so we remove them all with
        # the 'find' command below)

        cd $dir1
        echo "\naex: removing nested empty directories\n"
        find $dir2 -depth -type d -empty -exec rmdir -v {} \; 2>&1 | sed "s/^/  $c55>$cee /"
    fi

    echo "\naex: ($current/$#): \`$c1$f$ce' processed\n"
    # move back to our original working directory to process the next archive
    cd $dir0

    echo "\n$c6----------------------------------------$ce"
    let current=current+1 # use "let" to tell shell that we're working with integers, not strings
done

fs=""; ds=""
if [[ $# -eq 1 ]]; then
    fs="\`$c1$1$ce'"
    ds="directory"
else
    fs="all files $c1$@$ce"
    ds="directories"
fi
echo "\naex: $fs extracted successfully"
echo "aex: new extracted-to $ds:"
for dir_new in $fbs; do
    echo "  $c5$dir_new$ce"
done
echo

# vim: syntax=zsh
