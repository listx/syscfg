#!/bin/zsh

# [minute] [hour] [day of the month] [month] [day of the week] [command]
# (command section can be prepended with "root" to execute as root)

#------------------------------------------------------------------------------#
# every hour, check if we are online (try to connect to wikipedia), and if so, #
# update the pacman package updates                                            #
#------------------------------------------------------------------------------#

# we assign different update times for different systems so that we don't step on each other's toes when updating
min=""
case $HOST in
    exelion)    min="53" ;;
    luxion)     min="47" ;;
    aether)     min="37" ;;
    forest)     min="27" ;;
    ocean)      min="14" ;;
    *)          min="7" ;;
esac
pacman="$min * * * * ping -c 4 -W 10 wikipedia.org && sudo bauerbill -Syy --rebase"

#---------------------------------------------------------------#
# give westminster clock sounds (without hour count) every hour #
#---------------------------------------------------------------#
westminster=""
if [[ $HOST == "exelion" ]]; then
    westminster="\n0 * * * * ~/syscfg/shellscripts/alarm/westminster-nohour.sh"
fi

echo "$pacman$westminster" | crontab -
