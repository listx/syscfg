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

exec ~/.local/bin/xmonad
