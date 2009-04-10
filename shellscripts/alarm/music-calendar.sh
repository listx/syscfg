#!/bin/bash
# A simple bash script to wake me up.

# display welcome message
echo "おはようございます！"
echo ""

# display today's date in color
echo -n "                            "
echo -en "\033[1;37m`date +%a`\033[0m " # Thu,
echo -en "\033[1;36m`date +%B`\033[0m " # March
echo -en "\033[1;33m`date +%d`\033[0m " # 06
echo -e  "\033[1;32m`date +%Y`\033[0m"  # 2009

# display calendar
pal -f /home/listdata/syscfg/pal/exelion.cfg

# unmute if sound is muted, and set the volume to 0
amixer -q set Master 0 unmute

ncmpcpp play

# this for loop takes 30s to set volume from 70 to 100
for ((i = 70; i <= 100; i++)) do
    amixer -q set Master $i\%
    # sleep for 1 second
    sleep 1s
done

#--------------------------------------------------------------------------------------------------#
# About the *read* command:                                                                        #
# from http://stackoverflow.com/questions/92802/what-is-the-linux-equivalent-to-dos-pause          #
#                                                                                                  #
# read -n1 -r -p "Press any key to continue..." key                                                #
#                                                                                                  #
# The -n1 specifies that it only waits for a single character. The -r puts it into raw mode, which #
# is necessary because otherwise, if you press something like backslash, it doesn't register until #
# you hit the next key. The -p specifies the prompt, which must be quoted if it contains spaces.   #
# The key argument is only necessary if you want to know which key they pressed, in which case you #
# can access it through $key.                                                                      #
#                                                                                                  #
# If you are using bash, you can also specify a timeout with -t, which causes read to return a     #
# failure when a key isn't pressed. So for example:                                                #
#                                                                                                  #
# read -t5 -n1 -r -p "Press any key in the next five seconds..." key                               #
# if [ $? -eq 0 ]; then                                                                            #
#     echo A key was pressed.                                                                      #
# else                                                                                             #
#     echo No key was pressed.                                                                     #
# fi                                                                                               #
#--------------------------------------------------------------------------------------------------#

echo ""
read -n1 -r -p "Press any key to exit this message... "
