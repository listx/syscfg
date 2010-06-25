#!/bin/zsh

# WHITE ON BLACK (more or less) theme; very zenburn-ish

export TERM_COLOR_SCHEME_CURRENT='wb'

c_cursor="#00ff00"    # cursor color

c_bg="#22222a" # background
c_00="#22222a" # black
c_08="#709080" # black bright

c_fg="#cccccf" # foreground
c_07="#cccccf" # white
c_15="#ffffff" # white bright

c_01="#d78787" # red regular
c_09="#d70000" # red bright

c_02="#60b48a" # green
c_10="#87ff87" # green bright

c_04="#94bff3" # blue
c_12="#94bff3" # blue bright

c_03="#d7d7af" # yellow
c_11="#ffd7af" # yellow bright

c_06="#8cd0d3" # cyan
c_14="#afffff" # cyan bright

c_05="#dc8cc3" # magenta
c_13="#d787af" # magenta bright

# make these colors available to any "printf '\33]...\007'" statement from a shell script (urxvt-only)
export TERM_COLOR_CURSOR=$c_cursor
export TERM_COLOR_BG=$c_bg
export TERM_COLOR_FG=$c_fg

colors=""
colors+=" --cursorColor $c_cursor"
colors+=" --background $c_bg"
colors+=" --foreground $c_fg"
colors+=" --color0 $c_00 --color1 $c_01 --color2 $c_02 --color3 $c_03 --color4 $c_04 --color5 $c_05 --color6 $c_06 --color7 $c_07 --color8 $c_08 --color9 $c_09 --color10 $c_10 --color11 $c_11 --color12 $c_12 --color13 $c_13 --color14 $c_14 --color15 $c_15"

urxvt ${(z)colors} & disown

# vim:syntax=zsh
