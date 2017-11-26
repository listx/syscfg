#!/usr/bin/env bash

for f in "$HOME"/syscfg/launchctl/*.plist; do
    ln -sfv "$f" "$HOME"/Library/LaunchAgents
done
