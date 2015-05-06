#!/usr/bin/env zsh
# Git tagging script

# Usage: gtag <PROGRAM-NAME> <VERSION-NUMBER>

# Note: <VERSION-NUMBER> should only be the version number itself; this script
# will add "v" as a prefix when tagging it with git.

msg=$1

if [[ -z "$msg" ]]; then
    echo "empty commit message -- aborting"
    exit 1
fi

ver=$(echo $msg | cut -d ' ' -f2)

git commit -am "$msg"
git tag -a "v$ver" -m "$msg"
