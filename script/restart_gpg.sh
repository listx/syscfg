#!/usr/bin/env zsh

setopt ERR_EXIT
setopt NO_UNSET
setopt PIPE_FAIL
setopt XTRACE

unsetopt ERR_EXIT
pkill gpg-agent
setopt ERR_EXIT

gpg-agent --daemon
