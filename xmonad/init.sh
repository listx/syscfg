#!/usr/bin/env zsh

# initialize X keyboard/mouse settings
~/syscfg/xmonad/xenv.sh

# Start ssh-agent.
if [[ -z "$SSH_AUTH_SOCK" ]]; then
    eval $(ssh-agent)
fi

# Start xscreensaver.
xscreensaver -nosplash &

# Manually source .zprofile, to load in additional PATHs (including, e.g.,
# ~/.xmonad). This manual step is necessary because the zsh session that loads
# this script is not a login shell and thus does not automatically read
# ~/.zprofile.
source ~/.zprofile

exec ~/.xmonad/xmonad
