#!/bin/zsh

if [[ -e /proc/asound/card1 ]]; then
    ln -sf ~/syscfg/alsa/cfg-total-bithead ~/.asoundrc
else
    case $HOST in
    k0) ln -sf ~/syscfg/alsa/cfg-k0 ~/.asoundrc ;;
    k1) ln -sf ~/syscfg/alsa/cfg-k1 ~/.asoundrc ;;
    *) echo "Unknown host \`$HOST'" ;;
    esac
fi
