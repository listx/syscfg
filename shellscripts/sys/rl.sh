#!/bin/zsh
# Program: rl (for Reverse (symbolic) Link)
# Author: Linus Arver
# Date: 2010
# Usage: rln SOURCE(S) DESTINATION_DIRECTORY

# This script is useful when browsing inside a deeply nested folder, and the user wants to symlink a number of these
# files quickly to a directory, without using something like `ln -s $PWD/file1 $PWD/file2 $PWD/file3 DEST', but instead,
# just: `rln file1 file2 file3 DEST'. This way, the user can quickly use tab-completion and go about his business,
# instead of tediously typing out the full path of every single target file.

# check for errors
if [[ $# -lt 2 ]]; then
    echo "error: need at least 1 source file followed by 1 destination directory"
    exit 1
fi

if [[ ! -d $@[-1] ]]; then
    echo "error: last argument is not a valid directory"
    exit 1
fi

# check if all given source files are valid files, and that links to them don't already exist in the destination
# directory
for i in {1..$(($# - 1))}; do
    if [[ ! -f $PWD/$@[$i] ]]; then
        echo "error: source file \`$@[$i]\' is not a valid file under the current directory"
        exit 1
    elif [[ -a $@[$#]/$@[$i] ]]; then
        echo "error: destination file $@[$#]/$@[$i] already exists"
        exit 1
    fi
done

# perform the symlinking
for i in {1..$(($# - 1))}; do
    echo "creating symlink $@[$#]/$@[$i] -> $PWD/$@[$i]"
    ln -s $PWD/$@[$i] $@[$#] # ln will spit out errors if it encounters any
done

# vim: syntax=zsh
