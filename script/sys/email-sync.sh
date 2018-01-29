#!/usr/bin/env bash

offlineimap -a main & pid1=$!

wait $pid1
echo "Synced at $(date)"
