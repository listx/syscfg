#!/usr/bin/env bash

# References.
# https://apple.stackexchange.com/questions/10467/how-to-increase-keyboard-key-repeat-rate-on-os-x
# https://stackoverflow.com/questions/43868520/how-to-adjust-keyrepeat-and-repeatdelay-in-osx-sierra

# Based on our own testing, "InitialKeyRepeat" is how long we wait, and
# "KeyRepeat" is the rate of keys that are repeated. For "InitialKeyRepeat", we
# assume that each unit is equivalent to 15ms, based on the comment of 15 ==
# 225ms.
defaults write -g InitialKeyRepeat -float 16.67 # normal minimum is 15 (225 ms)
defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)
