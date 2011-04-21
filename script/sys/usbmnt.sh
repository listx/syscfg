#!/bin/zsh
# Program: Try to mount a single-partition USB flash drive.
# Author: Linus Arver
# Date: 2011

zmodload zsh/pcre

pcre_compile "sd([^\\s])"

fstype=$1

if [[ -n $2 ]]; then
    dev=sd$2
else
    # e.g., get the 'c' in "sdc"
    devLine=$(dmesg | grep "Attached SCSI removable" | tail -n 1)
    pcre_match $devLine
    if [[ $#match -gt 0 ]]; then
        dev=sd$match
    fi
fi

# Now mount it to /mnt/usb, depending on fstype
if [[ $fstype == "ext2" ]]; then
    sudo mount -t ext2 -o rw,relatime /dev/${dev}1 /mnt/usb
else # try the vfat filesystem otherwise
    sudo mount -t vfat -o rw,uid=$USER,gid=$USER /dev/${dev}1 /mnt/usb
fi

# vim: syntax=zsh
