#!/bin/zsh
# Program: Try to mount a single-partition USB flash drive.
# Author: Linus Arver
# Date: 2011

fstype=$1

# Get the very latest usb-inserted device node; it will be "[sdb]" or "[sdc]", etc.
devBracket=$(dmesg | grep "Attached SCSI removable" | tail -n 1 | cut -d " " -f 3)
dev=$devBracket[2,4] # e.g., extract "sdb" out of "[sdb]"

# Now mount it to /mnt/usb, depending on fstype
if [[ $fstype == "ext2" ]]; then
    sudo mount -t ext2 -o rw,relatime /dev/${dev}1 /mnt/usb
else # try the vfat filesystem otherwise
    sudo mount -t vfat -o rw,uid=$USER,gid=$USER /dev/${dev}1 /mnt/usb
fi

# vim: syntax=zsh
