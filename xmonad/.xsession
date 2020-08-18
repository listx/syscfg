#!/usr/bin/env zsh

export PATH=~/.xmonad:$PATH
xcompmgr -c -r 0 &
exec xmonad
