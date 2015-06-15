#!/usr/bin/env zsh
# Set up keyboard in a sane manner.

# We don't background any of these X API calls to ensure a deterministic manner
# of execution.

# set keyboard layout to us-intl (altgr variant)
setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl -option "terminate:ctrl_alt_bksp"

# Convert Caps_Lock into Hyper_L key, and also map it to "mod3" modifier group.
# Default xmodmap output (vanilla state):
#
#   xmodmap:  up to 4 keys per modifier, (keycodes in parentheses):
#
#   shift       Shift_L (0x32),  Shift_R (0x3e)
#   lock        Caps_Lock (0x42)
#   control     Control_L (0x25),  Control_R (0x69)
#   mod1        Alt_L (0x40),  Meta_L (0xcd)
#   mod2        Num_Lock (0x4d)
#   mod3
#   mod4        Super_L (0x85),  Super_R (0x86),  Super_L (0xce),  Hyper_L (0xcf)
#   mod5        ISO_Level3_Shift (0x5c),  Mode_switch (0xcb)

# Prevent pressing Caps_Lock (0x42) from triggering the "Lock" modifier.
xmodmap -e "remove Lock = Caps_Lock"

# At this stage, pressing Caps_Lock still behaves like the original. To prevent
# this from happening, make it so that pressing Caps_Lock (hardware keycode
# 0x42, decimal 66) results in activating a different key event in X. In
# particular, we want it to trigger a "Hyper_L" event.
xmodmap -e "keysym Caps_Lock = Hyper_L"

# At this point, pressing Caps_Lock will be treated exactly the same as a
# Hyper_L keypress. You can test it by running 'xev' and pressing Caps_Lock.

# Now the goal is to move Hyper_L from mod4 to mod3. First remove Hyper_L from
# mod4.
xmodmap -e "remove mod4 = Hyper_L"
# Hyper_L is now "free", so add it to mod3.
xmodmap -e "add mod3 = Hyper_L"

# On w1 (Ubuntu VM), we actually run it as a guest VM from Mac, and on the Mac,
# we set Caps_Lock to behave as Scroll_Lock (because it is supported by the
# "Seil" program. Hyper_L does not exist on OSX, it seems. But scroll lock
# does, and that's what we use (since MacBook Air keyboard does not come with
# scroll lock, we can use it.
if [[ $HOST == "w1" ]]; then
    xmodmap -e "keysym Scroll_Lock = Hyper_L"
fi

# set keystroke repeat speed (delay, speed)
xset r rate 250 80
