CFG := $(shell pwd)
HOST := $(shell hostname)
# add the -n flag for directories, as otherwise, stray symlinks will be created inside the CFG directory itself
all: acpi boxes cron emacs git gsy lesskey modprobe mpd mplayer mutt ncmpcpp pal pentadactyl rtorrent usbmnt vim xdefaults xinitrc xmodmap xmonad xorg zsh
acpi:
	ln -fs ${CFG}/acpi/${HOST}.sh						/etc/acpi/handler.sh
boxes:
	ln -fs ${CFG}/boxes/cfg								${HOME}/.boxes
cron:
	${CFG}/cron/setcron.sh
emacs:
	ln -fs ${CFG}/emacs									${HOME}/.emacs.d
	ln -fs ${CFG}/emacs/cfg.el							${HOME}/.emacs
git:
	ln -fs ${CFG}/git/cfg								${HOME}/.gitconfig
	ln -fs ${CFG}/git/sendemail-aliases					${HOME}/.git-sendemail-aliases
gsy:
	ln -fs ${CFG}/gsy/cfg								${HOME}/.gsy
lesskey:
	ln -fs ${CFG}/lesskey/cfg							${HOME}/.lesskey
	lesskey
modprobe:
	sudo cp ${CFG}/modprobe/blacklist.conf				/etc/modprobe.d
ifeq ('${HOST}','k0')
	sudo cp ${CFG}/modprobe/modprobe.k0.conf			/etc/modprobe.d/modprobe.conf
endif
ifeq ('${HOST}','k1')
	sudo cp ${CFG}/modprobe/modprobe.k1.conf			/etc/modprobe.d/modprobe.conf
endif
ifeq ('${HOST}','k2')
	sudo cp ${CFG}/modprobe/modprobe.k2.conf			/etc/modprobe.d/modprobe.conf
endif
mplayer:
	ln -fns ${CFG}/mplayer								${HOME}/.mplayer
mutt:
	ln -fns ${CFG}/mutt									${HOME}/.mutt
	ln -fs ${CFG}/mutt/cfg								${HOME}/.muttrc
	ln -fs ${CFG}/mutt/mailcap							${HOME}/.mailcap
ncmpcpp:
	ln -fns ${CFG}/ncmpcpp								${HOME}/.ncmpcpp
	ln -fs ${CFG}/ncmpcpp/hotkeys						${HOME}/.ncmpcpp/keys
ifeq ('${HOST}','k0')
	ln -fs ${CFG}/ncmpcpp/cfg							${HOME}/.ncmpcpp/config
else
	ln -fs ${CFG}/ncmpcpp/cfg-k2						${HOME}/.ncmpcpp/config
endif
pal:
	ln -fns ${CFG}/pal									${HOME}/.pal
	ln -fs ${CFG}/pal/cfg								${HOME}/.pal/pal.conf
pentadactyl:
	ln -fs ${CFG}/pentadactyl/cfg						${HOME}/.pentadactylrc
rf:
	ln -fs ${CFG}/rf/cfg								${HOME}/.rf
rtorrent:
ifeq ('${HOST}','k0')
	ln -fs ${CFG}/rtorrent/cfg							${HOME}/.rtorrent.rc
endif
ifeq ('${HOST}','k1')
	ln -fs ${CFG}/rtorrent/cfg-k1						${HOME}/.rtorrent.rc
endif
ifeq ('${HOST}','k3')
	ln -fs ${CFG}/rtorrent/cfg						${HOME}/.rtorrent.rc
endif
usbmnt:
	ln -fs ${CFG}/usbmnt/cfg							${HOME}/.usbmnt
vim:
	ln -fns ${CFG}/vim									${HOME}/.vim
	ln -fs ${CFG}/vim/cfg								${HOME}/.vimrc
	ln -fs ${CFG}/vim/cfg								${HOME}/.gvimrc
xdefaults:
	ln -fs ${CFG}/xdefaults/cfg							${HOME}/.Xdefaults
xinitrc:
	ln -fs ${CFG}/xinitrc/cfg							${HOME}/.xinitrc
xmodmap:
	ln -fs ${CFG}/xmodmap/cfg							${HOME}/.xmodmap
xmonad:
	ln -fns ${CFG}/xmonad								${HOME}/.xmonad
xorg:
	ln -fns ${CFG}/xorg/10-keyboard.conf				/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/10-server-flags.conf			/etc/X11/xorg.conf.d/
ifeq ('${HOST}','k0')
	ln -fns ${CFG}/xorg/10-dual-monitor-portrait.conf		/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/50-mouse.conf					/etc/X11/xorg.conf.d/
endif
ifeq ('${HOST}','k2')
	ln -fns ${CFG}/xorg/50-synaptics.conf				/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/20-intel-video-k2.conf				/etc/X11/xorg.conf.d/
endif
ifeq ('${HOST}','k1')
	ln -fns ${CFG}/xorg/50-synaptics.conf				/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/20-intel-video.conf				/etc/X11/xorg.conf.d/
endif
ifeq ('${HOST}','k3')
	ln -fns ${CFG}/xorg/10-quad-monitor-portrait.conf		/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/50-mouse.conf					/etc/X11/xorg.conf.d/
endif
ifeq ('${HOST}','forest')
	ln -fns ${CFG}/xorg/10-dual-monitor-hybrid-forest.conf	/etc/X11/xorg.conf.d/
endif
zsh:
	mkdir ${HOME}/.zsh-untracked
	ln -fns ${CFG}/zsh									${HOME}/.zsh
	ln -fs ${CFG}/zsh/cfg								${HOME}/.zshrc

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
	rm ${HOME}/.Xdefaults
	rm ${HOME}/.xinitrc
	rm ${HOME}/.xmonad/init.sh
	rm ${HOME}/.xmonad/xmonad.hs
	rm ${HOME}/.xmonad
	rm ${HOME}/.zsh
	rm ${HOME}/.zshrc

# NOTE: /boot/grub/menu.lst, /etc/inittab, /etc/rc.local and /etc/fstab are read
# in that order BEFORE any other disks are mounted; this means that these files
# cannot be symlinked from our repo as our repo will not be available at these
# early stages of booting.
core:
ifeq ('${HOST}','k0')
	cat /boot/grub/menu.lst >	${CFG}/core/boot-grub-menu.lst-k0
	cat /etc/hostname.conf >	${CFG}/core/etc-hostname-k0
	cat /etc/fstab >			${CFG}/core/etc-fstab-k0
	cat /etc/vconsole.conf >	${CFG}/core/etc-vconsole.conf-k0
	ln -fns ${CFG}/core/etc-hosts-k0					/etc/hosts
	ln -fns ${CFG}/core/etc-locale.conf					/etc/locale.conf
	ln -fns ${CFG}/core/etc-makepkg.conf-k0				/etc/makepkg.conf
	ln -fns ${CFG}/core/etc-modules-load.d-load.conf-k0	/etc/modules-load.d/load.conf
endif
ifeq ('${HOST}','k2')
	cat /boot/syslinux/syslinux.cfg >	${CFG}/core/boot-syslinux-syslinux.cfg-k2
	cat /etc/hostname.conf >	${CFG}/core/etc-hostname-k2
	cat /etc/fstab >					${CFG}/core/etc-fstab-k2
	cat /etc/vconsole.conf >			${CFG}/core/etc-vconsole.conf-k2
	ln -fns ${CFG}/core/etc-hosts-k2					/etc/hosts
	ln -fns ${CFG}/core/etc-locale.conf					/etc/locale.conf
	ln -fns ${CFG}/core/etc-makepkg.conf-k2				/etc/makepkg.conf
	ln -fns ${CFG}/core/etc-modules-load.d-load.conf-k2	/etc/modules-load.d/load.conf
endif
ifeq ('${HOST}','k1')
	cat /boot/grub/menu.lst >	${CFG}/core/boot-grub-menu.lst-k1
	cat /etc/hostname.conf >	${CFG}/core/etc-hostname-k1
	cat /etc/fstab >			${CFG}/core/etc-fstab-k1
	cat /etc/vconsole.conf >	${CFG}/core/etc-vconsole.conf-k1
	ln -fns ${CFG}/core/etc-hosts-k1					/etc/hosts
	ln -fns ${CFG}/core/etc-locale.conf					/etc/locale.conf
	ln -fns ${CFG}/core/etc-makepkg.conf-k1				/etc/makepkg.conf
	ln -fns ${CFG}/core/etc-modules-load.d-load.conf-k1	/etc/modules-load.d/load.conf
endif
