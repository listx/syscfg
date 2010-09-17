#!/bin/zsh

# negative decibels instead of percentage:
#   amixer get Master | tail -n1 | cut -d " " -f7 | sed -e "s/\..\+//" -e "s/[^0-9]//g"

# First detect Master sound channel's volume; the higher it is, the lower we play back with timidity's own --volume
# flag.

# $vol's range is 0-100
vol=$(amixer get Master | tail -n1 | cut -d " " -f6 | sed "s/[^0-9]//g")
tvol=0

if [[ $vol -eq 0 ]]; then
    exit # quit, since volume of 0 indicates that the user does not want to be disturbed
elif [[ $vol -lt 51 ]]; then
    # for volumes 1 through 50, just play with max 800 amplification (ideally, we want volume to be at least 50)
    tvol=800
else
    case $vol in
        52) tvol=780 ;;
        53) tvol=750 ;;
        55) tvol=700 ;;
        56) tvol=660 ;;
        58) tvol=600 ;;
        59) tvol=550 ;;
        61) tvol=450 ;;
        62) tvol=420 ;;
        63) tvol=400 ;;
        64) tvol=380 ;;
        66) tvol=350 ;;
        67) tvol=320 ;;
        69) tvol=300 ;;
        70) tvol=275 ;;
        72) tvol=250 ;;
        73) tvol=210 ;;
        75) tvol=190 ;; # GOOD
        77) tvol=170 ;;
        78) tvol=150 ;;
        80) tvol=140 ;; # GOOD
        81) tvol=128 ;;
        83) tvol=120 ;;
        84) tvol=110 ;;
        86) tvol=100 ;;
        88) tvol=95 ;;
        89) tvol=90 ;;
        91) tvol=80 ;;
        92) tvol=65 ;;
        94) tvol=50 ;; # GOOD
        95) tvol=38 ;;
        97) tvol=32 ;;
        98) tvol=30 ;; # GOOD
        100) tvol=25 ;;

        *) # this should never happen
            tvol=100
        ;;
    esac
fi

case $1 in
    "a") timidity --volume $tvol ~/syscfg/sys/sound/00a-westminster-FX3_ReverbMax.mid ;;
    "b") timidity --volume $tvol ~/syscfg/sys/sound/00b-westminster-FX3_ReverbMax.mid ;;
    "f") timidity --volume $tvol ~/syscfg/sys/sound/00-westminster-FX3_ReverbMax.mid ;;
    *) ;;
esac

# vim: syntax=zsh
