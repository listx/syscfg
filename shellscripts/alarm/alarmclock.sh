#!/bin/zsh
# A simple script to wake me up.

# colors
c1="\x1b[1;38;5;120m" # bright green
c2="\x1b[1;38;5;228m" # bright yellow
c3="\x1b[1;38;5;214m" # bright orange
c4="\x1b[1;38;5;159m" # bright cyan
c5="\x1b[1;38;5;175m" # bright purple
c6="\x1b[1;38;5;160m" # bright red
ce="\x1b[0m"

# display welcome message
echo "alarmclock: Good morning!"
# display today's date in color
echo "alarmclock: The time is now $c1$(date +%A)$ce $c2$(date +%B)$ce $c3$(date +%-e)$ce $c4$(date +%T)$ce\n"

# display calendar
pal -f /home/listdata/syscfg/pal/cfg | sed "s/^/  /"

echo "\nalarmclock: Press 'q' to turn off the music and exit"
echo "alarmclock: Press 'k' to just exit"

# save old sound settings (this is soundcard-specific)
orig_vol=$(amixer get Master | tail -n 1 | cut -d " " -f 6 | sed 's/[[:punct:]]//g')
orig_pwr=$(amixer get Master | tail -n 1 | cut -d " " -f 8 | sed 's/[[:punct:]]//g')

# unmute if sound is muted, and set the volume to 0
amixer -q set Master 0 unmute

# start playing song in current playlist
ncmpcpp play

# modified fibonacci sequence (10 numbers)
fibs=(5 10 20 30 50 80 130 210 340 550)

# initialize volume array
vols=()
for i in {30..100}; do
    vols+=($i)
done

key=""
# set volume starting from 30 to 100 (70 calls), but gradually
x=0
while true; do
    amixer -q set Master $vols[1]\%
    shift vols
    let x++
    # sleep longer with each iteration (somewhat exponentially)
    for i in {1..$fibs[1]}; do
        read -s -t 0.1 -k key
        case $key in
            "q")
                echo "alarmclock: Exiting..."
                # stop music and restore old values
                ncmpcpp pause
                amixer -q set Master "$orig_vol"% unmute
                [[ $orig_pwr == "off" ]] && amixer -q set Master mute
                exit
                ;;
            "k")
                echo "alarmclock: Exiting..."
                exit
                ;;
            "") ;; # don't display anything if user does not press anything
            *)
                echo "alarmclock: Unrecognized character"
                ;;
        esac
    done

    # every 7th call, we move up a notch in the fibs array (for a total of 70 calls)
    if (( x % 7 == 0 )); then
        shift fibs
    fi

    # quit if we're done (volume is at max level)
    if [[ -z $fibs ]]; then
        # since user did not quit by this time (after many minutes of loud
        # music), quit automatically, but turn off the music
        echo "alarmclock: Exiting..."
        ncmpcpp pause
        amixer -q set Master "$orig_vol"% unmute
        [[ $orig_pwr == "off" ]] && amixer -q set Master mute
        exit
    fi
done

# vim:syntax=zsh
