#!/bin/zsh

if [[ -e /proc/asound/card1 ]]; then
    ln -sf ~/syscfg/alsa/cfg-total-bithead ~/.asoundrc
else
    case $HOST in
    k0|k1|k2|k3) ln -sf ~/syscfg/alsa/cfg-$HOST ~/.asoundrc ;;
    *) echo "Unknown host \`$HOST'" ;;
    esac
fi
