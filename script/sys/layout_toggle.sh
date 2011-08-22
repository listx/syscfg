#!/bin/zsh
# toggle between us (default) and us (altgr-intl) layouts

variant=$(setxkbmap -query | grep variant | awk '{print $2}')
if [[ $variant == "altgr-intl" ]]; then
    setxkbmap -rules evdev -model evdev -layout us -option "terminate:ctrl_alt_bksp"
else
    setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl -option "terminate:ctrl_alt_bksp"
fi

# remap Caps_Lock key to Xmonad's exclusive 'mod' key
~/syscfg/script/sys/xmodmap.sh

# vim: syntax=zsh
