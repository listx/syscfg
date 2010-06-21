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
str+="$min * * * * ping -c 4 -W 10 wikipedia.org && sudo bauerbill -Syy --rebase"

# westminster clock sounds (without hour count)
# full bell every 3rd hour (hours 0,3,6,9,12,15,18,21)
# half-bells (different flavors) for morning/midday and evening
if [[ $HOST == "exelion" ]]; then
    str+="\n0 0,3,6,9,12,15,18,21 * * * ~/syscfg/shellscripts/alarm/westminster-nohour.sh f"
    str+="\n0 1,2,4,5,7,8,10,11,13,14 * * * ~/syscfg/shellscripts/alarm/westminster-nohour.sh a"
    str+="\n0 16,17,19,20,22,23 * * * ~/syscfg/shellscripts/alarm/westminster-nohour.sh b"
    # set alarm clock to go off
    str+="\n55 4 * * * urxvt -hold -e ~/syscfg/shellscripts/alarm/alarmclock.sh"
    # run "mercury routine" every minute to check for all routine tasks (recurring todo lists)
    str+="\n*/1 * * * * ~/sched/check.sh routine"
    # display the todo list three times a day
    str+="\n0 6,12,18 * * * ~/sched/check.sh todo"
    # display all notes once a day in the evening
    str+="\n0 18 * * * ~/sched/check.sh notes"
fi

echo "$str" | crontab -
