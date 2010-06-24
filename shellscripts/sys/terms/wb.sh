#!/bin/zsh

# WHITE ON BLACK (more or less) theme; very zenburn-ish

# LESS's settings (for manpages with color!)
# 38;5;xxxm is the 256-color palette for foreground, and 48;5;xxxm is the pattern for the background (see urxvt(7), "Character Attributes")
export LESS_TERMCAP_md=$'\E[1;38;5;39m'    # begin bold (most common color)
export LESS_TERMCAP_us=$'\E[1;38;5;214m'    # begin underline (second most common)
export LESS_TERMCAP_so=$'\E[38;5;21;48;5;226m' # begin standout-mode - (search highlight)
export LESS_TERMCAP_mb=$'\E[1;31;5;196;5m'    # begin blinking (the last "5" actually makes it blink) (this is hardly ever used by manpages...)
export LESS_TERMCAP_me=$'\E[0m'       # end bold/blinking
export LESS_TERMCAP_se=$'\E[0m'       # end standout-mode
export LESS_TERMCAP_ue=$'\E[0m'       # end underline


urxvt & disown

# vim:syntax=zsh
