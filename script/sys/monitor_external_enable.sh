#!/bin/zsh
# enable external VGA monitor

monitor_external=$(xrandr | grep VGA1)
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

xrandr --output LVDS1 --auto --output VGA1 --auto --$external_location LVDS1

# vim:syntax=zsh
