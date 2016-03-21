#!/usr/bin/env zsh

internal="LVDS"
external="HDMI-0"

if [[ -n $(xrandr | grep "$external" | grep "+") ]]; then
    xrandr --output $external --off --output $internal --auto
    else if [[ -n $(xrandr | grep "$external" | grep " connected") ]]; then
        xrandr --output $internal --off --output $external --auto
    fi
fi
