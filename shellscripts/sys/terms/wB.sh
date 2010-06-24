#!/bin/zsh

# WHITE ON BLUE theme (late 80's-ish, almost videogame-like look)

export TERM_COLOR_SCHEME_CURRENT='wB'

# white on blue colors
colors=""
colors+=" --background #3560ce --color0 #3560ce --color8 #4073f5" # background/black regular/black bold
colors+=" --foreground #ffffff --color7 #ffffff --color15 #ffffff" # foreground/text regular/text bold
colors+=" --color1 #ffa020 --color9 #ffa020" # red (regular, bold)
colors+=" --color2 #5bde54 --color10 #5bde54" # green
colors+=" --color4 #0c2b6a --color12 #133377" # blue
colors+=" --color3 #ffdb00 --color11 #ffdb00" # yellow
colors+=" --color6 #00dbde --color14 #22ffff" # cyan

urxvt ${(z)colors} & disown

# vim:syntax=zsh
