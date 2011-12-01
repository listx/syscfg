#!/bin/zsh

# [minute] [hour] [day of the month] [month] [day of the week] [command]
# (command section can be prepended with "root" to execute as root)

#------------------------------------------------------------------------------#
# every hour, check if we are online (try to connect to wikipedia), and if so, #
# update the pacman package updates                                            #
#------------------------------------------------------------------------------#

str=""

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
str+="$min 12 * * * ping -c 4 -W 10 wikipedia.org && sudo pacman -Syy"

# westminster clock sounds (without hour count)
# full bell every 6th hour (hours 0,6,12,18)
# half-bells (different flavors) for morning/midday and evening
if [[ $HOST == "exelion" ]]; then
    str+="\n0 0,12 * * * ~/syscfg/script/alarm/westminster-nohour.sh f"
    str+="\n0 1,2,3,4,5,6,7,8,9,10,11 * * * ~/syscfg/script/alarm/westminster-nohour.sh a"
    str+="\n0 13,14,15,16,17,18,19,20,21,22,23 * * * ~/syscfg/script/alarm/westminster-nohour.sh b"
fi

if [[ $HOST == "luxion" || $HOST == "aether" ]]; then
    # check if battery life is below 20% every 5 minutes
    str+="\n*/5 * * * * ~/syscfg/script/sys/batlife.sh"
fi

echo "$str" | crontab -
