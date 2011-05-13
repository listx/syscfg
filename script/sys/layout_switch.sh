#!/bin/zsh
# switch between us, fr, and de layouts

# If an explicit layout is provided as an argument, use it. Otherwise, select the next layout from
# the set [us, fr, de].
if [[ -n "$1" ]]; then
    setxkbmap $1
else
    layout=$(setxkbmap -query | awk 'END{print $2}')
    case $layout in
        us)
            setxkbmap fr
            ;;
        fr)
            setxkbmap de
            ;;
        *)
            setxkbmap us
            ;;
    esac
fi

# remap Caps_Lock key to Xmonad's exclusive 'mod' key
~/syscfg/script/sys/xmodmap.sh

# vim: syntax=zsh
