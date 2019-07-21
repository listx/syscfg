#!/bin/sh
#
# Set up keyboard and X environment.
#
# The most importnat thing we do here is that we disable Caps_Lock from behaving
# like a "Lock" key, convert it to behave as a Hyper_L key, and then add Hyper_L
# to its own unique modifer group, mod3 (mod3 is unused by default). We use mod3
# as our XMonad key as "mod3Mask" in xmonad.hs.
#
# We don't background any of these X API calls to ensure a deterministic manner
# of execution.

# Set keyboard layout to us-intl (altgr variant). This way we can type letters
# like "é" easily.
setxkbmap \
	-rules evdev \
	-model evdev \
	-layout us \
	-variant altgr-intl \
	-option "terminate:ctrl_alt_bksp"

# Convert Caps_Lock into Hyper_L key, and also map it to "mod3" modifier group
# (used exclusively by XMonad, because virtually no userspace programs make use
# of it). The nice side effect is that the Caps Lock key becomes impossible to
# press from our keyboard.
#
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

# Now the goal is to move Hyper_L from mod4 to mod3. This way, Hyper_L does not
# share the same modifier namespace as the Super_L (Windows) key. First remove
# Hyper_L from mod4.
xmodmap -e "remove mod4 = Hyper_L"
# Hyper_L is now "free", so add it to mod3.
xmodmap -e "add mod3 = Hyper_L"

# Set keystroke repeat speed (delay, speed).
xset r rate 250 80

# We are done with setting up the keyboard. Below are misc non-keyboard
# X-related settings.

# Disable mouse acceleration.
xset m 0 0

# Use circle, not "X" icon, for the mouse cursor on empty workspaces (an area
# without windows).
xsetroot -cursor_name left_ptr

# For NixOS machines, manually start up xscreensaver in the background, if it is
# not running already.
if < /etc/issue | grep NixOS -q \
	&& ! ps acx | grep xscreensaver -q; then
	xscreensaver -nosplash & disown
fi
