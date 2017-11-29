#!/usr/bin/env zsh

setopt ERR_EXIT
setopt NO_UNSET
setopt PIPE_FAIL

stack build
stack install
