#!/bin/zsh
# Git tagging script

msg=$1

if [[ -z "$msg" ]]; then
    echo "empty commit message -- aborting"
    exit 1
fi

ver=$(echo $msg | cut -d ' ' -f2)

git commit -am "$msg"
git tag -a "v$ver" -m "$msg"

# vim: syntax=zsh
