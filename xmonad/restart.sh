#!/bin/sh

set -ex

# We assume that xmonad has been recompiled already by the time this script is
# called.
stack exec -- xmonad --restart
