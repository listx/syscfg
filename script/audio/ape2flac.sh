#!/usr/bin/env zsh

setopt errexit

for f in *.ape; do
	wav="${f:r}.wav"
	mac "$f" $wav -d
	mac "$f" -v
done

flac --verify --replay-gain --best *.wav
