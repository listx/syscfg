#!/bin/zsh

# black on white (off-white)

export LESS_TERMCAP_md=$'\E[1;38;5;118m'    # begin bold
export LESS_TERMCAP_us=$'\E[1;38;5;205m'    # begin underline
export LESS_TERMCAP_so=$'\E[38;5;21;48;5;226m' # begin standout-mode - (search highlight)
export LESS_TERMCAP_mb=$'\E[1;31;5;196;5m'    # begin blinking (the last "5" actually makes it blink)
export LESS_TERMCAP_me=$'\E[0m'       # end bold/blinking
export LESS_TERMCAP_se=$'\E[0m'       # end standout-mode
export LESS_TERMCAP_ue=$'\E[0m'       # end underline

export TERM_COLOR_SCHEME_CURRENT='bw'

colors=""
colors+=" --background #cccccc --color0 #cccccc --color8 #dddddd" # background/black regular/black bold
colors+=" --foreground #000000 --color7 #000000 --color15 #000000" # foreground/text regular/text bold
# colors+=" --color1 #ffa020 --color9 #ffa020" # red (regular, bold)
# colors+=" --color2 #5bde54 --color10 #5bde54" # green
# colors+=" --color4 #0c2b6a --color12 #133377" # blue
colors+=" --color3 #cccc33 --color11 #cccc33" # yellow
# colors+=" --color6 #00dbde --color14 #22ffff" # cyan

urxvt ${(z)colors} & disown

# vim:syntax=zsh
