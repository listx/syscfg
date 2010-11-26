#!/bin/zsh
# check battery life

batlife=$(acpi | awk '{print $NF}' | sed 's/\%//')

if (( $batlife < 20 )); then
    urxvt -hold -e zsh -c "echo warning: Battery life is $batlife%"
fi

# vim:syntax=zsh
