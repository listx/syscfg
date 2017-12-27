#!/usr/bin/env zsh

setopt ERR_EXIT
setopt NO_UNSET
setopt PIPE_FAIL

# We assume that xmonad has been recompiled already by the time this script is
# called.
stack exec -- xmonad --restart
