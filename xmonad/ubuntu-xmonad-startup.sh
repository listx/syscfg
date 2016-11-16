#!/usr/bin/env zsh

# This file should be symlinked as ~/.xmonad/xmonad-session-rc. See
# http://unix.stackexchange.com/questions/162448/xsession-and-xinitrc-not-executed-at-login.

# initialize keyboard in a sane manner
~/syscfg/script/sys/initkeys.sh

# Disable touchpad tapping.
synclient TapButton1=0

# Swap left/right mouse buttons.

if [[ "$(hostname)" == larver-w0 ]]; then
    xmodmap -e "pointer = 3 2 1"
fi
