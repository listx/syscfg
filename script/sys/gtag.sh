#!/usr/bin/env zsh
# Git tagging script

# Usage: gtag <PROGRAM_NAME> <VERSION_NUMBER>

# Note: <VERSION_NUMBER> should only be the version number itself; this script
# will add "v" as a prefix when tagging it with git.

program_name=$1
version_number=$2

if [[ -z $program_name || -z $version_number ]]; then
	echo "usage: gtag.sh <PROGRAM_NAME> <VERSION_NUMBER>"
	exit 1
fi

git commit -am "$program_name $version_number"
git tag -a "v$version_number" -m "$program_name $version_number"
