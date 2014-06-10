#!/bin/zsh

link="$HOME/.asoundrc"
linkDest=$(basename $(readlink -f "$link"))

if [[ $linkDest == cfg-total-bithead ]]; then
    case $HOST in
    k0|k1|k2) ln -sfv ~/syscfg/alsa/cfg-$HOST $link ;;
    *) echo "Unknown host \`$HOST'" ;;
    esac
else
    ln -sfv ~/syscfg/alsa/cfg-total-bithead $link
fi
