#!/usr/bin/env bash

offlineimap -a main & pid1=$!
offlineimap -a work & pid2=$!

wait $pid1
wait $pid2
echo "Synced at $(date)"
