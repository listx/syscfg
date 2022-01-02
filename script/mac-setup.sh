#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

# Disable creation of .DS_Store files from Finder.
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool TRUE
