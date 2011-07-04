#!/bin/zsh
# ssr (Silent Sustained Reading!) turns off the internet connection for the given amount of time and
# backgrounds itself; hopefully, during this time the user will not slack off on the internet and
# get some real work done, and build confidence in his own abilities as a problem solver

. ~/.zshrc

if [[ -z "$1" ]]; then
    amount="2h"
else
    amount="$1"
fi

net_seal
sleep $amount
net_unseal

# vim: syntax=zsh
