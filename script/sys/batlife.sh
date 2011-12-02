#!/usr/bin/env zsh
# check battery life

batlife=$(acpi | sed 's/\%.\+//' | awk '{print $NF}')

if (( $batlife < 20 )); then
    urxvt -hold -e zsh -c "echo warning: Battery life is $batlife%"
fi

# vim:syntax=zsh
