#!/bin/zsh
# decrypt files painlessly

if [[ $1 == "outfile" ]]; then
    gpg2 -o "$(basename "$2" ".gpg")" -d "$2"
elif [[ $1 == "viewfile" ]]; then
    gpg2 -d "$2" | less
else
    echo "error: ivalid mode \`$1'"
fi

# vim:syntax=zsh
