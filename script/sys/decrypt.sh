#!/bin/zsh
# decrypt files painlessly

gpg2 -o "$(basename "$1" ".gpg")" -d "$1"

# vim:syntax=zsh
