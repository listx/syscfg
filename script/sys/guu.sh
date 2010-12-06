#!/bin/zsh
# Program: guu (Git Update to USB)
# Author: Linus Arver
# Date: 2010
# Usage: guu [parente directory of repos in USB]

# colors
c1="\x1b[1;32m" # bright green
c2="\x1b[1;33m" # bright yellow
c3="\x1b[1;34m" # bright blue
c4="\x1b[1;36m" # bright cyan
c5="\x1b[1;35m" # bright purple
c6="\x1b[1;31m" # bright red
ce="\x1b[0m"

# set default usb directory
usb=/mnt/media-flash
repodir=$1

if [[ -z $(df | grep $usb) ]]; then
    echo "error: no USB drive detected"
    exit 1
elif [[ -z $repodir ]]; then
    echo "usage: guu [parent directory of repos in USB]"
    exit 1
elif [[ ! -d /mnt/media-flash/"$repodir" ]]; then
    echo "error: no parent directory of repositories specified"
    exit 1
fi

cd /mnt/media-flash/"$repodir"

for subdir in $(ls -C1); do
    cd $subdir
    echo "/mnt/media-flash/$repodir/${c2}$subdir$ce"
    git pull 2>&1 | sed -e "s/^/  $c1>$ce /" -e "s/error/${c6}error$ce/"
    echo
    cd ..
done

echo "${c1}All repos updated.$ce"

# vim: syntax=zsh
