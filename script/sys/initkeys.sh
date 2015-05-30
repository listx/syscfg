#!/usr/bin/env zsh
# Set up keyboard in a sane manner.

# We don't background any of these X API calls to ensure a deterministic manner
# of execution.

# set keyboard layout to us-intl (altgr variant)
setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl -option "terminate:ctrl_alt_bksp"

# Xmodmap. Run `xmodmap` without arguments to see the mappings. These are the
# default settings on Ubuntu 14.04.01:
#
# xmodmap:  up to 4 keys per modifier, (keycodes in parentheses):
#
# shift       Shift_L (0x32),  Shift_R (0x3e)
# lock        Super_L (0x42)
# control     Control_L (0x25),  Control_R (0x69)
# mod1        Alt_L (0x40),  Alt_R (0x6c),  Meta_L (0xcd)
# mod2        Num_Lock (0x4d)
# mod3
# mod4        Super_L (0x85),  Super_R (0x86),  Super_L (0xce),  Hyper_L (0xcf)
# mod5        ISO_Level3_Shift (0x5c),  Mode_switch (0xcb)

if [[ $HOST == "vbox" ]]; then
    xmodmap -e "remove Lock = Caps_Lock"
    xmodmap -e "keysym Caps_Lock = Hyper_L"
    xmodmap -e "remove mod4 = Hyper_L"
    xmodmap -e "add mod3 = Hyper_L"
    xmodmap -e "keysym Scroll_Lock = Hyper_L"
else
    # Legacy Linux-only settings.
    # FIXME: do not rely on buggy behavior!

    # remap Caps_Lock key to Xmonad's exclusive 'mod' key
    xmodmap -e "remove Lock = Caps_Lock"
    #xmodmap -e "clear mod3" <--- unnecessary b/c 'xmodmap' returns BLANK for mod3 on default settings
    xmodmap -e "add mod3 = Caps_Lock"
    xmodmap ~/.xmodmap
fi

# set keystroke repeat speed (delay, speed)
xset r rate 250 80
