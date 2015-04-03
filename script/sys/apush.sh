#!/usr/bin/env zsh

# push files to android device

for f in $@; do
	adb push "$f" /sdcard/Download
done
