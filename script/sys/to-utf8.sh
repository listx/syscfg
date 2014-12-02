#!/usr/bin/env zsh

from=$1
shift

for f in $@; do
	iconv -f $from -t utf-8 $f > ${f:r}.srt
done
