#!/bin/zsh
hour=`date +%I` # get current hour, in 01-12 format
`timidity --volume 800 ~/syscfg/sys/sound/$hour-westminster-FX3_ReverbMax.mid`
