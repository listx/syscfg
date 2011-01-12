#!/bin/zsh
# Program: rouse
# Version: 1.0
# Author: Linus Arver
# License: GPLv3

# Copyright (C) 2010 Linus Arver

# rouse: a script that checks which systems are offline, and asks user which systems to wake up via Wake-on-Lan

# Colors (comment out to disable)
c1="\x1b[1;32m" # bright green
c2="\x1b[1;33m" # bright yellow
c3="\x1b[1;34m" # bright blue
c4="\x1b[1;36m" # bright cyan
c5="\x1b[1;35m" # bright purple
c6="\x1b[1;31m" # bright red
ce="\x1b[0m"

machines=(luxion aether exelion forest ocean)
laptops=(luxion aether)
remotes=()
online=()
offline=()
lanip="192.168.0.255"

# Populate remotes_foreign
for m in $machines; do
    if [[ $m != $HOST ]]; then
        # if the remote is recognized by laptops() array, connect with an
        # appended ".e"
        [[ -n ${laptops[(r)$m]} ]] && m+=".e"
        remotes+=($m)
    fi
done

echo "Checking LAN status...\n"
# Check which systems are offline
for r in $remotes; do
    ping -c 1 -W 1 $r >/dev/null 2>&1
    if [[ $? -eq 0 ]]; then
        online+=($r)
    else
        offline+=($r)
    fi
done

# Display those systems that are online
i=0
for r in $remotes; do
    ((i++))
    if [[ -n ${online[(r)$r]} ]]; then
        echo $i\) $r "->" ${c1}online$ce
    else
        echo $i\) $r "->" ${c6}offline$ce
    fi
done

# If there are offline systems, enter interactive menu
if [[ $#offline -gt 0 ]]; then
    echo "\nChoose system to wake (q to exit):\n"
    while [[ $#offline -gt 0 ]]; do
        read -s -t 1 -k key
        case $key in
        1|2|3|4|5|6|7|8|9)
            system=$remotes[$key]
            if [[ -z $system ]]; then
                echo "Choice $key invalid"
            elif [[ -n ${online[(r)$system]} ]]; then
                echo "$system is already online"
            else
                echo "Chose system ${c3}$system$ce"
                case $system in
                exelion)
                    wol -i $lanip 00:04:4B:02:51:47
                    ;;
                aether.e)
                    wol -i $lanip 00:23:26:5C:07:37
                    ;;
                luxion.e)
                    wol -i $lanip 00:12:3F:05:85:FE
                    ;;
                forest)
                    # Gigabyte GA-P35-DS3L does not support wake-on-lan (WOL) with its own built-in ethernet ports; you
                    # have to buy a separate NIC to get it working
                    echo "Unfortunately, system ${c3}$system$ce does not support WOL"
                    ;;
                ocean)
                    wol -i $lanip 00:50:8D:BC:9B:72
                    ;;
                *)
                    echo "System \`$system' not yet supported"
                    ;;
                esac
                offline[$offline[(i)$system]]=()
                echo
            fi
            ;;
        q) exit ;;
        *) ;;
        esac
        key=""
    done
else
    echo "All systems online."
fi

# vim: syntax=zsh
