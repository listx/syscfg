- in Apple keyboard settings, under "Modifier Keys", make CapsLock do nothing.
- install Seil
- map CapsLock to right command (ubuntu vm will see this Super_R in `xev` program)
- in Ubuntu, modify the `Exec` line in `/usr/share/xsessions/xmonad.desktop` from
    Exec=xmonad
to
    Exec=~/.xinitrc
