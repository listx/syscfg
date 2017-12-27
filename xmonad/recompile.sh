#!/usr/bin/env zsh

setopt ERR_EXIT
setopt NO_UNSET
setopt PIPE_FAIL

# Compile our xmonad.hs.
stack build

# Install the xmonad binary to the local path. The fact that we use
# `${HOME}/.xmonad' here is mainly for reference purposes, so that we can at a
# glance compare the binary size/timestamp of `xmonad' versus the one generated
# below (for debugging).
stack --local-bin-path=${HOME}/.xmonad install

# Use the above binary to recompile itself --- this is the step that installs
# the odd-looking xmonad-x86_64-linux to ~/.xmonad. We could probably just
# overwrite it ourselves manually with `cp -f', but this way we make stack's
# xmonad "do the right thing". Not to mention, the sizes of "xmonad" from the
# step above and "xmonad-x86_64-linux" from the step below differ by almost 1 MB
# --- so using `cp -f' isn't even the same thing.
#
# We are probably doing things in a sub-optimal way, but until a better method
# is found, so be it.
stack exec -- xmonad --recompile
