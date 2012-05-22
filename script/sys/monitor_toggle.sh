#!/bin/zsh

internal="LVDS1"
external="VGA1"

if [[ -n $(xrandr | grep "$external" | grep "+") ]]; then
    xrandr --output $external --off --output $internal --auto
    else if [[ -n $(xrandr | grep "$external" | grep " connected") ]]; then
        xrandr --output $internal --off --output $external --auto
    fi
fi

# vim:syntax=zsh
