#!/usr/bin/env bash

# References.
# https://apple.stackexchange.com/questions/53734/how-do-i-prevent-x11-opening-an-xterm-when-it-starts.
defaults write org.macosforge.xquartz.X11 app_to_run "$HOME/syscfg/script/terms/wb.sh"
