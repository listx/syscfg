#!/bin/sh

set -ex

nix-shell --command "./xmonad --restart"
