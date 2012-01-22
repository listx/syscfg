CFG := $(shell pwd)
HOST := $(shell hostname)
# add the -n flag for directories, as otherwise, stray symlinks will be created inside the CFG directory itself
all: acpi boxes cron emacs galculator git gsy gtk lesskey modprobe mpd mplayer mutt ncmpcpp nixos pal pentadactyl rtorrent sh usbmnt vim xdefaults xinitrc xmodmap xmonad xorg zsh
acpi:
	ln -fs ${CFG}/acpi/${HOST}.sh		/etc/acpi/handler.sh
boxes:
	ln -fs ${CFG}/boxes/cfg			${HOME}/.boxes
cron:
	${CFG}/cron/setcron.sh
emacs:
	ln -fs ${CFG}/emacs			${HOME}/.emacs.d
	ln -fs ${CFG}/emacs/cfg			${HOME}/.emacs
git:
	ln -fs ${CFG}/git/cfg			${HOME}/.gitconfig
	ln -fs ${CFG}/git/sendemail-aliases	${HOME}/.git-sendemail-aliases
gsy:
	ln -fs ${CFG}/gsy/cfg			${HOME}/.gsy
galculator:
	ln -fs ${CFG}/galculator/cfg		${HOME}/.galculator
gtk:
	ln -fs ${CFG}/gtk/cfg			${HOME}/.gtkrc-2.0
lesskey:
	ln -fs ${CFG}/lesskey/cfg		${HOME}/.lesskey
	lesskey
modprobe:
	ln -fs ${CFG}/modprobe/blacklist.conf	/etc/modprobe.d
ifeq ('${HOST}','k0')
	ln -fs ${CFG}/modprobe/modprobe.k0.conf	/etc/modprobe.d
endif
ifeq ('${HOST}','k1')
	ln -fs ${CFG}/modprobe/modprobe.k1.conf	/etc/modprobe.d
endif
mplayer:
	ln -fns ${CFG}/mplayer			${HOME}/.mplayer
mutt:
	ln -fns ${CFG}/mutt			${HOME}/.mutt
	ln -fs ${CFG}/mutt/cfg			${HOME}/.muttrc
	ln -fs ${CFG}/mutt/mailcap		${HOME}/.mailcap
ncmpcpp:
	ln -fns ${CFG}/ncmpcpp			${HOME}/.ncmpcpp
	ln -fs ${CFG}/ncmpcpp/hotkeys		${HOME}/.ncmpcpp/keys
ifeq ('${HOST}','k0')
	ln -fs ${CFG}/ncmpcpp/cfg		${HOME}/.ncmpcpp/config
endif
ifeq ('${HOST}','k2')
	ln -fs ${CFG}/ncmpcpp/cfg-k2		${HOME}/.ncmpcpp/config
endif
ifeq ('${HOST}','k1')
	ln -fs ${CFG}/ncmpcpp/cfg-k2		${HOME}/.ncmpcpp/config
endif
nixos:
	ln -fs ${CFG}/nixos/base.nix		/etc/nixos
	ln -fs ${CFG}/nixos/${HOST}.nix		/etc/nixos/configuration.nix

pal:
	ln -fns ${CFG}/pal			${HOME}/.pal
	ln -fs ${CFG}/pal/cfg			${HOME}/.pal/pal.conf
pentadactyl:
	ln -fs ${CFG}/pentadactyl/cfg		${HOME}/.pentadactylrc
rtorrent:
ifeq ('${HOST}','k0')
	ln -fs ${CFG}/rtorrent/cfg		${HOME}/.rtorrent.rc
endif
ifeq ('${HOST}','k1')
	ln -fs ${CFG}/rtorrent/cfg-k1		${HOME}/.rtorrent.rc
endif
sh:
	ln -fns ${CFG}/sh/zshenv		/etc
usbmnt:
	ln -fs ${CFG}/usbmnt/cfg		${HOME}/.usbmnt
vim:
	ln -fns ${CFG}/vim			${HOME}/.vim
	ln -fs ${CFG}/vim/cfg			${HOME}/.vimrc
	ln -fs ${CFG}/vim/cfg			${HOME}/.gvimrc
xdefaults:
	ln -fs ${CFG}/xdefaults/cfg		${HOME}/.Xdefaults
xinitrc:
	ln -fs ${CFG}/xinitrc/cfg		${HOME}/.xinitrc
xmodmap:
	ln -fs ${CFG}/xmodmap/cfg		${HOME}/.xmodmap
xmonad:
	ln -fns ${CFG}/xmonad			${HOME}/.xmonad
xorg:
	ln -fns ${CFG}/xorg/10-keyboard.conf		/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/10-server-flags.conf	/etc/X11/xorg.conf.d/
ifeq ('${HOST}','k0')
	ln -fns ${CFG}/xorg/10-dual-monitor-hybrid.conf	/etc/X11/xorg.conf.d/
endif
ifeq ('${HOST}','k2')
	ln -fns ${CFG}/xorg/10-synaptics.conf		/etc/X11/xorg.conf.d/
endif
ifeq ('${HOST}','k1')
	ln -fns ${CFG}/xorg/10-synaptics.conf		/etc/X11/xorg.conf.d/
endif
zsh:
	mkdir ${HOME}/.zsh-untracked
	ln -fns ${CFG}/zsh			${HOME}/.zsh
	ln -fs ${CFG}/zsh/cfg			${HOME}/.zshrc

uninstall:
	rm ${HOME}/.boxes
	rm ${HOME}/.gitconfig
ifeq ('${HOST}','k0')
	rm ${HOME}/.mpd
endif
	rm ${HOME}/.mplayer
	rm ${HOME}/.ncmpcpp/config
	rm ${HOME}/.ncmpcpp/keys
	rm ${HOME}/.ncmpcpp
	rm ${HOME}/.pal/pal.conf
	rm ${HOME}/.pal
	rm ${HOME}/.vim
	rm ${HOME}/.vimrc
	rm ${HOME}/.gvimrc
	rm ${HOME}/.vimperatorrc
	rm ${HOME}/.Xdefaults
	rm ${HOME}/.xinitrc
	rm ${HOME}/.xmonad/init.sh
	rm ${HOME}/.xmonad/xmonad.hs
	rm ${HOME}/.xmonad
	rm ${HOME}/.zsh
	rm ${HOME}/.zshrc

# copy contents of system files to keep track of them
core:
ifeq ('${HOST}','k0')
	cat /boot/grub/menu.lst >         ${HOME}/syscfg/core/boot-grub-menu.lst-k0
	cat /etc/fstab >                  ${HOME}/syscfg/core/etc-fstab-k0
	cat /etc/hosts >                  ${HOME}/syscfg/core/etc-hosts-k0
	cat /etc/inittab >                ${HOME}/syscfg/core/etc-inittab-k0
	cat /etc/makepkg.conf >           ${HOME}/syscfg/core/etc-makepkg.conf-k0
	cat /etc/rc.conf >                ${HOME}/syscfg/core/etc-rc.conf-k0
	cat /etc/rc.local >               ${HOME}/syscfg/core/etc-rc.local-k0
	cat /etc/rc.local.shutdown >      ${HOME}/syscfg/core/etc-rc.local.shutdown-k0
	cat /etc/yaourtrc >               ${HOME}/syscfg/core/etc-yaourtrc-k0
	cat /etc/sudoers >                ${HOME}/syscfg/core/etc-sudoers-k0 # requires superuser privileges to read!
endif
ifeq ('${HOST}','k2')
	cat /boot/grub/menu.lst >         ${HOME}/syscfg/core/boot-grub-menu.lst-k2
	cat /etc/fstab >                  ${HOME}/syscfg/core/etc-fstab-k2
	cat /etc/hosts >                  ${HOME}/syscfg/core/etc-hosts-k2
	cat /etc/inittab >                ${HOME}/syscfg/core/etc-inittab-k2
	cat /etc/makepkg.conf >           ${HOME}/syscfg/core/etc-makepkg.conf-k2
	cat /etc/network.d/k2-wired >     ${HOME}/syscfg/core/etc-network.d-k2-wired
	cat /etc/rc.conf >                ${HOME}/syscfg/core/etc-rc.conf-k2
	cat /etc/rc.local >               ${HOME}/syscfg/core/etc-rc.local-k2
	cat /etc/rc.local.shutdown >      ${HOME}/syscfg/core/etc-rc.local.shutdown-k2
	cat /etc/yaourtrc >               ${HOME}/syscfg/core/etc-yaourtrc-k2
	cat /etc/sudoers >                ${HOME}/syscfg/core/etc-sudoers-k2
endif
ifeq ('${HOST}','k1')
	cat /boot/grub/menu.lst >         ${HOME}/syscfg/core/boot-grub-menu.lst-k1
	cat /etc/fstab >                  ${HOME}/syscfg/core/etc-fstab-k1
	cat /etc/hosts >                  ${HOME}/syscfg/core/etc-hosts-k1
	cat /etc/makepkg.conf >           ${HOME}/syscfg/core/etc-makepkg.conf-k1
	cat /etc/rc.conf >                ${HOME}/syscfg/core/etc-rc.conf-k1
	cat /etc/rc.local >               ${HOME}/syscfg/core/etc-rc.local-k1
	cat /etc/rc.local.shutdown >      ${HOME}/syscfg/core/etc-rc.local.shutdown-k1
endif

# vim: tabstop=8
