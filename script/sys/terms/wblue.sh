#!/usr/bin/env zsh

# WHITE ON BLUE theme (late 80's-ish, almost videogame-like look)

export TERM_COLOR_SCHEME_CURRENT='wB'

case $HOST in
	k1)
		geom="100x50"
		fonts="xft:Terminus:pixelsize=14,xft:IPAGothic,xft:Baekmuk Gulim"
	;;
	ocean)
		geom="80x20"
		fonts="xft:dejavu sans mono:pixelsize=36,xft:IPAGothic,xft:Baekmuk Gulim"
	;;
	*)
		geom="100x70"
		fonts="xft:Terminus:pixelsize=14,xft:IPAGothic,xft:Baekmuk Gulim"
	;;
esac

c_cursor="#ff00ff"    # cursor color

c_bg="#3560ce" # background
c_00="#3560ce" # black
c_08="#4073f5" # black bright

c_fg="#ffffff" # foreground
c_07="#ffffff" # white
c_15="#ffffff" # white bright

c_01="#ffa020" # red regular
c_09="#ffa020" # red bright

c_02="#5bde54" # green
c_10="#5bde54" # green bright

c_04="#0c2b6a" # blue
c_12="#133377" # blue bright

c_03="#ffdb00" # yellow
c_11="#ffdb00" # yellow bright

c_06="#00dbde" # cyan
c_14="#22ffff" # cyan bright

c_05="#dc8cc3" # magenta
c_13="#d787af" # magenta bright

export TERM_COLOR_CURSOR=$c_cursor
export TERM_COLOR_BG=$c_bg
export TERM_COLOR_FG=$c_fg

# white on blue colors
opts=""
opts+=" -geometry $geom"
opts+=" --cursorColor $c_cursor"
opts+=" --background $c_bg"
opts+=" --foreground $c_fg"
opts+=" --color0 $c_00 --color1 $c_01 --color2 $c_02 --color3 $c_03 --color4 $c_04 --color5 $c_05 --color6 $c_06 --color7 $c_07 --color8 $c_08 --color9 $c_09 --color10 $c_10 --color11 $c_11 --color12 $c_12 --color13 $c_13 --color14 $c_14 --color15 $c_15"

urxvt ${(z)opts} -fn $fonts $@ & disown
