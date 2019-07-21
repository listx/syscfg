#!/bin/sh

set -ex

# Install the xmonad binary to the local path.
stack --local-bin-path=${HOME}/.xmonad install

cp -f ${HOME}/.xmonad/{xmonad,xmonad-x86_64-linux}
