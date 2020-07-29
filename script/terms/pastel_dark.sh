#!/usr/bin/env zsh

export TERM_COLOR_THEME="pastel_dark.sh"

case $(uname) in
Linux)
	case $HOST in
	k1)
		geom="100x50"
		fonts="xft:Terminus:pixelsize=14,xft:IPAGothic,xft:Baekmuk Gulim"
	;;
	*)
		geom="100x70"
		fonts="xft:Terminus:pixelsize=14,xft:IPAGothic,xft:Baekmuk Gulim"
	;;
	esac
;;
Darwin)
	geom="80x55"
	fonts="xft:Input Mono Condensed:pixelsize=16,xft:IPAGothic,xft:Baekmuk Gulim"
;;
esac

c_cursor="#e5e7ea"    # cursor color

c_bg="#343c48" # background
#c_00="#627484" # black
c_00="#000000" # black
#c_08="#6986a0" # black bright
c_08="#000000" # black bright

c_fg="#e5e7ea" # foreground
c_07="#c1cad0" # white
c_15="#e5e7ea" # white bright

c_01="#e49f9f" # red regular
c_09="#e5bfbf" # red bright

c_02="#91e380" # green
c_10="#afe0a1" # green bright

c_04="#7cacd3" # blue
c_12="#95add1" # blue bright

c_03="#eae47c" # yellow
c_11="#f2fb9e" # yellow bright

c_06="#8cdbd8" # cyan
c_14="#b4f0f0" # cyan bright

c_05="#df9494" # magenta
c_13="#f2b0b0" # magenta bright

# make these colors available to any "printf '\33]...\007'" statement from a shell script (urxvt-only)
export TERM_COLOR_CURSOR=$c_cursor
export TERM_COLOR_BG=$c_bg
export TERM_COLOR_FG=$c_fg

opts=""
opts+=" -geometry $geom"
opts+=" --cursorColor $c_cursor"
opts+=" --background $c_bg"
opts+=" --foreground $c_fg"
opts+=" --color0 $c_00 --color1 $c_01 --color2 $c_02 --color3 $c_03 --color4 $c_04 --color5 $c_05 --color6 $c_06 --color7 $c_07 --color8 $c_08 --color9 $c_09 --color10 $c_10 --color11 $c_11 --color12 $c_12 --color13 $c_13 --color14 $c_14 --color15 $c_15"

urxvt ${(z)opts} -fn $fonts $@ & disown
