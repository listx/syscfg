CFG := $(shell pwd)
HOST := $(shell hostname)
# add the -n flag for directories, as otherwise, stray symlinks will be created inside the CFG directory itself
all: acpi boxes cron emacs git gsy gtk lesskey modprobe mpd mplayer mutt ncmpcpp pal pentadactyl rtorrent usbmnt vim xdefaults xinitrc xmodmap xmonad xorg zsh
acpi:
	ln -fs ${CFG}/acpi/${HOST}.sh		/etc/acpi/handler.sh
boxes:
	ln -fs ${CFG}/boxes/cfg			${HOME}/.boxes
cron:
	${CFG}/cron/setcron.sh
emacs:
	ln -fs ${CFG}/emacs			${HOME}/.emacs.d
	ln -fs ${CFG}/emacs/cfg.el		${HOME}/.emacs
git:
	ln -fs ${CFG}/git/cfg			${HOME}/.gitconfig
	ln -fs ${CFG}/git/sendemail-aliases	${HOME}/.git-sendemail-aliases
gsy:
	ln -fs ${CFG}/gsy/cfg			${HOME}/.gsy
gtk:
	ln -fs ${CFG}/gtk/cfg			${HOME}/.gtkrc-2.0
lesskey:
	ln -fs ${CFG}/lesskey/cfg		${HOME}/.lesskey
	lesskey
modprobe:
	sudo cp ${CFG}/modprobe/blacklist.conf	/etc/modprobe.d
ifeq ('${HOST}','k0')
	sudo cp ${CFG}/modprobe/modprobe.k0.conf	/etc/modprobe.d/modprobe.conf
endif
ifeq ('${HOST}','k1')
	sudo cp ${CFG}/modprobe/modprobe.k1.conf	/etc/modprobe.d/modprobe.conf
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
else
	ln -fs ${CFG}/ncmpcpp/cfg-k2		${HOME}/.ncmpcpp/config
endif
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
	ln -fns ${CFG}/xorg/10-monitor-k2.conf		/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/20-intel-video.conf		/etc/X11/xorg.conf.d/
endif
ifeq ('${HOST}','k1')
	ln -fns ${CFG}/xorg/10-synaptics.conf		/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/20-intel-video.conf		/etc/X11/xorg.conf.d/
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
	# NOTE: /boot/grub/menu.lst, /etc/inittab, and /etc/fstab are read in that
	# order BEFORE any other disks are mounted; this means that these files
	# cannot be symlinked from our repo as our repo will not be available at
	# these early stages of booting.
	cat /boot/grub/menu.lst >         ${CFG}/core/boot-grub-menu.lst-k0
	cat /etc/fstab >                  ${CFG}/core/etc-fstab-k0
	cat /etc/inittab >                ${CFG}/core/etc-inittab-k0
	ln -fns ${CFG}/core/etc-hosts-k0	/etc/hosts
	ln -fns ${CFG}/core/etc-hostname-k0	/etc/hostname
	ln -fns ${CFG}/core/etc-makepkg.conf-k0	/etc/makepkg.conf
	ln -fns ${CFG}/core/etc-rc.conf-k0	/etc/rc.conf
	ln -fns ${CFG}/core/etc-locale.conf	/etc/locale.conf
	ln -fns ${CFG}/core/etc-modules-load.d-load.conf-k0	/etc/modules-load.d/load.conf
	ln -fns ${CFG}/core/etc-timezone	/etc/timezone
	ln -fns ${CFG}/core/etc-vconsole.conf	/etc/vconsole.conf
	ln -fns ${CFG}/core/etc-rc.local-k0	/etc/rc.local
	ln -fns ${CFG}/core/etc-rc.local.shutdown-k0	/etc/rc.local.shutdown
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
	ln -fns ${CFG}/core/etc-hostname-k2	/etc/hostname
endif
ifeq ('${HOST}','k1')
	cat /boot/grub/menu.lst >         ${CFG}/core/boot-grub-menu.lst-k1
	cat /etc/fstab >                  ${CFG}/core/etc-fstab-k1
	cat /etc/inittab >                ${CFG}/core/etc-inittab-k1
	ln -fns ${CFG}/core/etc-hosts-k1	/etc/hosts
	ln -fns ${CFG}/core/etc-hostname-k1	/etc/hostname
	ln -fns ${CFG}/core/etc-makepkg.conf-k1	/etc/makepkg.conf
	ln -fns ${CFG}/core/etc-rc.conf-k1	/etc/rc.conf
	ln -fns ${CFG}/core/etc-locale.conf	/etc/locale.conf
	ln -fns ${CFG}/core/etc-modules-load.d-load.conf-k1	/etc/modules-load.d/load.conf
	ln -fns ${CFG}/core/etc-timezone	/etc/timezone
	ln -fns ${CFG}/core/etc-vconsole.conf	/etc/vconsole.conf
	ln -fns ${CFG}/core/etc-rc.local-k1	/etc/rc.local
	ln -fns ${CFG}/core/etc-rc.local.shutdown-k1	/etc/rc.local.shutdown
endif

# vim: tabstop=8
