#!/usr/bin/env zsh
# enable external HDMI screen

monitor_external=$(xrandr | grep HDMI-0)
external_location="right-of"

if [[ -n $monitor_external ]]; then
    case $1 in
        left)
            external_location="left-of"
        ;;
        right)
            external_location="right-of"
        ;;
        top|above)
            external_location="above"
        ;;
        bottom|below)
            external_location="below"
        ;;
        *)
        ;;
    esac
fi

xrandr --output LVDS --auto --output HDMI-0 --auto --$external_location LVDS
