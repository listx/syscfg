#!/bin/sh

# since xbacklight always returns a single floating point number, we need to
# convert it to an integer

float=`xbacklight`
int=${float/\.*}

if [ $int -eq 0 ]; then
    xbacklight -set 100
else
    xbacklight -set 0
fi
