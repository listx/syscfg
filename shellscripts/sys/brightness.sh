#!/bin/sh

# since xbacklight always returns a single floating point number, we need to
# convert it to an integer -- we do this manually by dropping the decimal point
# and everything after it with sed

perc=`xbacklight | sed 's/\..\+//'`

if [ $perc -eq 0 ]; then
    xbacklight -set 100
else
    xbacklight -set 0
fi
