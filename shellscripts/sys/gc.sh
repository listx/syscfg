#!/bin/zsh
# Program: gc (ghost clone --- clone all git bare repos with depth 1 into usb drive)
# Author: Linus Arver
# Date: 2010
# Usage: gc [0-9]

# Only a single option exists, to turn on compression. Call gc with a number 0 through 9 (9 for best, but slowest,
# compression). The xz utility is used for compression.

# set default usb directory
usb=/mnt/media-flash

if [[ -z $(df | grep $usb) ]]; then
    echo error: no USB drive detected
    exit 1
fi

repos=$(ls $HOME/ghost)

for r in ${(f)repos}; do
    # go to the repo and make a backup of it into USB drive
    cd $HOME/ghost/$r
    rm -vf $(basename $r .git).tar
    git archive -v -o $(basename $r .git).tar HEAD
    if [[ $# -gt 0 ]]; then
        level=$(echo $1 | grep "^[0-9]$")
        if [[ -n $level ]]; then
            xz -vfz$level $(basename $r .git).tar
            rsync -ahP --no-whole-file --inplace $(basename $r .git).tar.xz $usb
        fi
    else
        rsync -ahP --no-whole-file --inplace $(basename $r .git).tar $usb
    fi
done

# vim: syntax=zsh
