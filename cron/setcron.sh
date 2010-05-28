#!/bin/zsh

# [minute] [hour] [day of the month] [month] [day of the week] [command]
# (command section can be prepended with "root" to execute as root)

#------------------------------------------------------------------------------#
# every hour, check if we are online (try to connect to wikipedia), and if so, #
# update the pacman package updates                                            #
#------------------------------------------------------------------------------#

# we assign different update times for different systems so that we don't step on each other's toes when updating
pacman0=""
case $HOST in
    exelion)    pacman0="53" ;;
    luxion)     pacman0="47" ;;
    aether)     pacman0="37" ;;
    forest)     pacman0="27" ;;
    ocean)      pacman0="14" ;;
    *)          pacman0="7" ;;
esac
pacman1=" * * * * ping -c 4 -W 10 wikipedia.org && sudo bauerbill -Syy --rebase"

#---------------------------------------------------------------#
# give westminster clock sounds (without hour count) every hour #
#---------------------------------------------------------------#
westminster=""
if [[ $HOST == "exelion" ]]; then
    westminster="\n0 * * * * ~/syscfg/shellscripts/alarm/westminster-nohour.sh"
fi

echo "$pacman0$pacman1$westminster" | crontab -
