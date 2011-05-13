#!/bin/zsh
# remap Caps_Lock key to Xmonad's exclusive 'mod' key

xmodmap -e "remove Lock = Caps_Lock"
#xmodmap -e "clear mod3" <--- unnecessary b/c 'xmodmap' returns BLANK for mod3 on default settings
xmodmap -e "add mod3 = Caps_Lock"
xmodmap ~/.xmodmap

# vim: syntax=zsh
