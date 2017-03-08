#!/usr/bin/env zsh

# The (find-file ...) avoids showing "*scratch*" buffer on startup when invoking
# from emacsclient.
emacsclient --alternate-editor "" --tty \
    --eval '(find-file "'"$1"'")'
