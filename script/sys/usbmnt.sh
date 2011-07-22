#!/bin/zsh
# Program: Try to mount a single-partition USB flash drive.
# Author: Linus Arver
# Date: 2011

success() {
    echo "USB device /dev/sd${d}1 ($fmt) mounted at /mnt/usb" \
    && exit
}

if [[ $1 == "ext2" ]]; then
    cmd="ext2 -o rw,relatime"
    fmt=ext2
else
    cmd="vfat -o rw,uid=$USER,gid=$USER"
    fmt=vfat
fi

if [[ -n $2 ]]; then
    sudo mount -t ${(z)cmd} /dev/sd${2}1 /mnt/usb && success()
else
    devices=(b c d e f g h i j k l m n o p q r s t u v w x y z)
    for d in $devices; do
        sudo mount -t ${(z)cmd} /dev/sd${d}1 /mnt/usb &>/dev/null && success()
    done
    echo "devices sdb through sdz are already mounted"
    exit 1
fi

# vim: syntax=zsh
