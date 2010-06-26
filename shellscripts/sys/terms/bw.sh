#!/bin/zsh

# black on white (off-white)

export TERM_COLOR_SCHEME_CURRENT='bw'

c_cursor="#202020"    # cursor color

c_bg="#f0f0f0" # background
c_00="#f0f0f0" # black
c_08="#fafafa" # black bright

c_fg="#404040" # foreground
c_07="#404040" # white
c_15="#202020" # white bright

c_01="#a07060" # red regular
c_09="#d02010" # red bright

c_02="#508040" # green
c_10="#509030" # green bright

c_04="#607080" # blue
c_12="#5080b0" # blue bright

c_03="#c09030" # yellow
c_11="#c09030" # yellow bright

c_06="#70a0a0" # cyan
c_14="#70b0b0" # cyan bright

c_05="#605080" # magenta
c_13="#604090" # magenta bright

c_fade="10"

export TERM_COLOR_CURSOR=$c_cursor
export TERM_COLOR_BG=$c_bg
export TERM_COLOR_FG=$c_fg

colors=""
colors+=" --cursorColor $c_cursor"
colors+=" --background $c_bg"
colors+=" --foreground $c_fg"
colors+=" --color0 $c_00 --color1 $c_01 --color2 $c_02 --color3 $c_03 --color4 $c_04 --color5 $c_05 --color6 $c_06 --color7 $c_07 --color8 $c_08 --color9 $c_09 --color10 $c_10 --color11 $c_11 --color12 $c_12 --color13 $c_13 --color14 $c_14 --color15 $c_15"
colors+=" -fade $c_fade"

urxvt ${(z)colors} -fn "xft:dejavu sans mono:size=10,xft:Kochi Gothic,xft:Baekmuk Gulim" $@ & disown

# vim:syntax=zsh
