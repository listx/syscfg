#!/usr/bin/env zsh
# Set up keyboard in a sane manner.

# We don't background any of these X API calls to ensure a deterministic manner
# of execution.

# set keyboard layout to us-intl (altgr variant)
setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl -option "terminate:ctrl_alt_bksp"

# remap Caps_Lock key to Xmonad's exclusive 'mod' key
xmodmap -e "remove Lock = Caps_Lock"
#xmodmap -e "clear mod3" <--- unnecessary b/c 'xmodmap' returns BLANK for mod3 on default settings
xmodmap -e "add mod3 = Caps_Lock"
xmodmap ~/.xmodmap

# set keystroke repeat speed (delay, speed)
xset r rate 250 80

# vim: syntax=zsh
