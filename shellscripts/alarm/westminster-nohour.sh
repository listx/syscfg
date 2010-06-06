#!/bin/zsh

case $1 in
    "a") timidity --volume 800 ~/syscfg/sys/sound/00a-westminster-FX3_ReverbMax.mid ;;
    "b") timidity --volume 800 ~/syscfg/sys/sound/00b-westminster-FX3_ReverbMax.mid ;;
    "f") timidity --volume 800 ~/syscfg/sys/sound/00-westminster-FX3_ReverbMax.mid ;;
    *) ;;
esac
