#!/bin/sh

set -ex

# Install the xmonad binary to the local path. The fact that we use
# `${HOME}/.xmonad' here is mainly for reference purposes, so that we can at a
# glance compare the binary size/timestamp of `xmonad' versus the one generated
# below (for debugging).
stack --local-bin-path=${HOME}/.xmonad install

cp -f ${HOME}/.xmonad/{xmonad,xmonad-x86_64-linux}
