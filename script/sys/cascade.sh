#!/usr/bin/env zsh

# Given a file with the form
#    1
#    2
#    3
# this script premutes it into
#    3
#    2
#    3
#    1
#    2
#    3

input="$1"
output="$2"

if [[ -z $input || ! -f $input ]]; then
    echo "input file cannot be blank"
    exit 1
fi

if [[ -z $output ]]; then
    echo "output file must be specified"
    exit 1
fi

<$input while read line; do
    line_ary+=($line)
done

:>$output
num=-1
for line in $line_ary; do
    echo "${(F)line_ary[$num,-1]}" >> $output
    let num-=1
done

# vim:syntax=zsh
