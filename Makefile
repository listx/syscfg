CFG := $(shell pwd)
HOST := $(shell hostname)
# add the -n flag for directories, as otherwise, stray symlinks will be created inside the CFG directory itself
all: acpi boxes cron emacs git gpg gsy lesskey modprobe mpd mplayer mutt ncmpcpp pal pentadactyl rtorrent urxvt usbmnt vim xdefaults xinitrc xinitrc-ubuntu xmonad xorg zsh
acpi:
	ln -fs ${CFG}/acpi/${HOST}.sh						/etc/acpi/handler.sh
bash:
	ln -fs ${CFG}/bash/.bashrc								${HOME}
boxes:
	ln -fs ${CFG}/boxes/cfg								${HOME}/.boxes
bundle:
	mkdir ${HOME}/.bundle
	ln -fs ${CFG}/bundle/cfg							${HOME}/.bundle/config
cron:
	${CFG}/cron/setcron.sh
emacs:
	ln -fs ${CFG}/emacs									${HOME}/.emacs.d
git:
	ln -fs ${CFG}/git/cfg								${HOME}/.gitconfig
	ln -fs ${CFG}/git/sendemail-aliases					${HOME}/.git-sendemail-aliases
gpg:
	ln -fs ${CFG}/gpg                                   ${HOME}/.gnupg
gsy:
	ln -fs ${CFG}/gsy/cfg								${HOME}/.gsy
gtk:
	ln -fs ${CFG}/gtk/cfg								${HOME}/.gtkrc-2.0.mine
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
mpv:
	ln -fns ${CFG}/mpv ${HOME}/.config/mpv
mupen:
	ln -fs ${CFG}/mupen/InputAutoCfg.ini				/usr/share/mupen64plus
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
nixos:
	ln -fs ${CFG}/nixos/${HOST}/configuration.nix	    /etc/nixos
	ln -fs ${CFG}/nixos/${HOST}/hardware-configuration.nix	    /etc/nixos
nixpkgs:
	ln -fns ${CFG}/nixpkgs									${HOME}/.nixpkgs
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
	ln -fs ${CFG}/rtorrent/cfg							${HOME}/.rtorrent.rc
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
xmonad:
	ln -fns ${CFG}/xmonad								${HOME}/.xmonad
xmonad-ubuntu:
	ln -fs ${CFG}/xmonad/xmonad-start.desktop           /usr/share/xsessions/
	cp ${CFG}/xinitrc/cfg							/usr/local/bin/xmonad-start
	chmod +x /usr/local/bin/xmonad-start
xsession:
	ln -fs ${CFG}/xsession/cfg                          ${HOME}/.xsession
xorg:
	ln -fns ${CFG}/xorg/10-keyboard.conf				/etc/X11/xorg.conf.d/
	ln -fns ${CFG}/xorg/10-server-flags.conf			/etc/X11/xorg.conf.d/
ifeq ('${HOST}','k0')
	ln -fns ${CFG}/xorg/10-quad-monitor-portrait.conf		/etc/X11/xorg.conf.d/
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
ifeq ('${HOST}','forest')
	ln -fns ${CFG}/xorg/10-dual-monitor-hybrid-forest.conf	/etc/X11/xorg.conf.d/
endif
zsh:
	ln -fns ${CFG}/zsh									${HOME}/.zsh
	ln -fs ${CFG}/zsh/.zshrc								${HOME}
	mkdir ${HOME}/.zsh-untracked
urxvt:
	ln -fns ${CFG}/urxvt                                ${HOME}/.urxvt

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

core:
ifeq ('${HOST}','k0')
	cat /boot/syslinux/syslinux.cfg >	${CFG}/core/boot-syslinux-syslinux.cfg-k0
	cat /etc/hostname >	${CFG}/core/etc-hostname-k0
	cat /etc/fstab >			${CFG}/core/etc-fstab-k0
	cat /etc/vconsole.conf >	${CFG}/core/etc-vconsole.conf-k0
endif
ifeq ('${HOST}','k2')
	cat /boot/syslinux/syslinux.cfg >	${CFG}/core/boot-syslinux-syslinux.cfg-k2
	cat /etc/hostname >	${CFG}/core/etc-hostname-k2
	cat /etc/fstab >					${CFG}/core/etc-fstab-k2
	cat /etc/vconsole.conf >			${CFG}/core/etc-vconsole.conf-k2
endif
ifeq ('${HOST}','k1')
	cat /boot/syslinux/syslinux.cfg >	${CFG}/core/boot-syslinux-syslinux.cfg-k1
	cat /etc/hostname >	${CFG}/core/etc-hostname-k1
	cat /etc/fstab >			${CFG}/core/etc-fstab-k1
	cat /etc/vconsole.conf >	${CFG}/core/etc-vconsole.conf-k1
endif
