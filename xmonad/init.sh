#!/usr/bin/env zsh

# initialize keyboard in a sane manner
~/syscfg/script/sys/initkeys.sh

# Change mouse to be left-handed.
xmodmap -e "pointer = 3 2 1"

# Start ssh-agent.
if [[ -z "$SSH_AUTH_SOCK" ]]; then
    eval $(ssh-agent)
fi

# Start xscreensaver.
xscreensaver -nosplash &

# Manually source .zprofile, to load in additional PATHs (including, e.g.,
# ~/.local/bin). This manual step is necessary because the zsh session that
# loads this script is not a login shell and thus does not automatically read
# ~/.zprofile.
source ~/.zprofile

exec ~/.local/bin/xmonad
