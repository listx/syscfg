#!/usr/bin/env bash

# Run xset when we start XQuartz (this is the only known way to change xset
# settings automatically when we start XQuartz). See
# https://superuser.com/questions/1006505/where-to-put-x11-xquartz-configuration-on-os-x.
pkill -9 xquartz-keys-setup
$HOME/.xinitrc.d/xquartz-keys-setup &
