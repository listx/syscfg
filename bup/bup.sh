#!/bin/zsh
# backup of home directory, but excluding (1) a list of manual files and (2) a dynamically-generated
# list of dotfiles

manual=$1
dots=/tmp/excludes-dots

# remove all current dotfiles from being backed up
for f in /home/listdata/.*; do
    echo $f
done > $dots

# backup source
backup_src=$2

# backup destination
backup_dest=$3

# store increment to backup folder
bup index -uv --exclude-from $manual --exclude-from $dots $backup_src
BUP_DIR=$backup_dest bup save -n exelion-home $backup_src

# vim:syntax=zsh
