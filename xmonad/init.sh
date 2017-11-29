#!/usr/bin/env zsh

# initialize keyboard in a sane manner
~/syscfg/script/sys/initkeys.sh

# Change mouse to be left-handed.
xmodmap -e "pointer = 3 2 1"

exec ~/.local/bin/xmonad
