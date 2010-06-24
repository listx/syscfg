#!/bin/zsh

# black on white (off-white)

export TERM_COLOR_SCHEME_CURRENT='bw'

colors=""
colors+=" --background #f0f0f0 --color0 #f0f0f0 --color8 #fafafa" # background/black regular/black bold
colors+=" --foreground #404040 --color7 #404040 --color15 #202020" # foreground/text regular/text bold
colors+=" --color1 #a07060 --color9 #d02010" # red (regular, bold)
colors+=" --color2 #508040 --color10 #509030" # green
colors+=" --color4 #607080 --color12 #5080b0" # blue
colors+=" --color3 #c09030 --color11 #c09030" # yellow
colors+=" --color6 #70a0a0 --color14 #70b0b0" # cyan
colors+=" --color5 #605080 --color13 #604090" # magenta

urxvt ${(z)colors} -fn "xft:dejavu sans mono:size=10,xft:Kochi Gothic,xft:Baekmuk Gulim" & disown

# vim:syntax=zsh
