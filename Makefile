CFGROOT := $(shell pwd)
HOSTNAME := $(shell hostname)
# add the -n flag for directories, as otherwise, stray symlinks will be created inside the CFGROOT directory itself
all: acpi boxes cron emacs galculator git gsy lesskey modprobe mpd mplayer mutt ncmpcpp nixos pal pentadactyl rtorrent sh usbmnt vim xdefaults xinitrc xmodmap xmonad xorg zsh
acpi:
	ln -fs $(CFGROOT)/acpi/${HOSTNAME}.sh /etc/acpi/handler.sh
boxes:
	ln -fs $(CFGROOT)/boxes/cfg         ${HOME}/.boxes
cron:
	$(CFGROOT)/cron/setcron.sh
emacs:
	ln -fs $(CFGROOT)/emacs             ${HOME}/.emacs.d
	ln -fs $(CFGROOT)/emacs/cfg         ${HOME}/.emacs
git:
	ln -fs $(CFGROOT)/git/cfg           ${HOME}/.gitconfig
	ln -fs $(CFGROOT)/git/sendemail-aliases ${HOME}/.git-sendemail-aliases
gsy:
	ln -fs $(CFGROOT)/gsy/cfg           ${HOME}/.gsy
galculator:
	ln -fs $(CFGROOT)/galculator/cfg    ${HOME}/.galculator
lesskey:
	ln -fs $(CFGROOT)/lesskey/cfg    	${HOME}/.lesskey
	lesskey
modprobe:
	sudo cp $(CFGROOT)/modprobe/blacklist.conf	/etc/modprobe.d
mplayer:
	ln -fns $(CFGROOT)/mplayer          ${HOME}/.mplayer
mutt:
	ln -fs $(CFGROOT)/mutt              ${HOME}/.mutt
	ln -fs $(CFGROOT)/mutt/cfg          ${HOME}/.muttrc
	ln -fs $(CFGROOT)/mutt/mailcap      ${HOME}/.mailcap
ncmpcpp:
	ln -fns $(CFGROOT)/ncmpcpp          ${HOME}/.ncmpcpp
	ln -fs $(CFGROOT)/ncmpcpp/hotkeys   ${HOME}/.ncmpcpp/keys
ifeq ('$(HOSTNAME)','k0')
	ln -fs $(CFGROOT)/ncmpcpp/cfg       ${HOME}/.ncmpcpp/config
endif
ifeq ('$(HOSTNAME)','k2')
	ln -fs $(CFGROOT)/ncmpcpp/cfg-k2 ${HOME}/.ncmpcpp/config
endif
ifeq ('$(HOSTNAME)','k1')
	ln -fs $(CFGROOT)/ncmpcpp/cfg-k2 ${HOME}/.ncmpcpp/config
endif
nixos:
	ln -fs $(CFGROOT)/nixos/base.nix			/etc/nixos
	ln -fs $(CFGROOT)/nixos/$(HOSTNAME).nix		/etc/nixos/configuration.nix

pal:
	ln -fns $(CFGROOT)/pal              ${HOME}/.pal
	ln -fs $(CFGROOT)/pal/cfg           ${HOME}/.pal/pal.conf
pentadactyl:
	ln -fs $(CFGROOT)/pentadactyl/cfg  ${HOME}/.pentadactylrc
rtorrent:
ifeq ('$(HOSTNAME)','k0')
	ln -fs $(CFGROOT)/rtorrent/cfg       ${HOME}/.rtorrent.rc
endif
ifeq ('$(HOSTNAME)','k1')
	ln -fs $(CFGROOT)/rtorrent/cfg-k1 ${HOME}/.rtorrent.rc
endif
sh:
	ln -fns $(CFGROOT)/sh/profile		${HOME}/.profile
usbmnt:
	ln -fs $(CFGROOT)/usbmnt/cfg  ${HOME}/.usbmnt
vim:
	ln -fns $(CFGROOT)/vim              ${HOME}/.vim
	ln -fs $(CFGROOT)/vim/cfg           ${HOME}/.vimrc
	ln -fs $(CFGROOT)/vim/cfg           ${HOME}/.gvimrc
xdefaults:
	ln -fs $(CFGROOT)/xdefaults/cfg     ${HOME}/.Xdefaults
xinitrc:
	ln -fs $(CFGROOT)/xinitrc/cfg       ${HOME}/.xinitrc
xmodmap:
	ln -fs $(CFGROOT)/xmodmap/cfg           ${HOME}/.xmodmap
xmonad:
	ln -fns $(CFGROOT)/xmonad           ${HOME}/.xmonad
ifeq ('$(HOSTNAME)','k0')
	ln -fs $(CFGROOT)/xmonad/others.hs           ${HOME}/.xmonad/xmonad.hs
endif
ifeq ('$(HOSTNAME)','k2')
	ln -fs $(CFGROOT)/xmonad/k2.hs           ${HOME}/.xmonad/xmonad.hs
endif
ifeq ('$(HOSTNAME)','k1')
	ln -fs $(CFGROOT)/xmonad/others.hs           ${HOME}/.xmonad/xmonad.hs
endif
xorg:
	ln -fns $(CFGROOT)/xorg/10-keyboard.conf	/etc/X11/xorg.conf.d/
	ln -fns $(CFGROOT)/xorg/10-server-flags.conf	/etc/X11/xorg.conf.d/
ifeq ('$(HOSTNAME)','k0')
	ln -fns $(CFGROOT)/xorg/10-dual-monitor-hybrid.conf		/etc/X11/xorg.conf.d/
endif
ifeq ('$(HOSTNAME)','k2')
	ln -fns $(CFGROOT)/xorg/10-synaptics.conf	/etc/X11/xorg.conf.d/
endif
ifeq ('$(HOSTNAME)','k1')
	ln -fns $(CFGROOT)/xorg/10-synaptics.conf	/etc/X11/xorg.conf.d/
endif
zsh:
	mkdir ${HOME}/.zsh-untracked
	ln -fns $(CFGROOT)/zsh              ${HOME}/.zsh
	ln -fs $(CFGROOT)/zsh/cfg           ${HOME}/.zshrc

uninstall:
	rm ${HOME}/.boxes
	rm ${HOME}/.gitconfig
ifeq ('$(HOSTNAME)','k0')
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
ifeq ('$(HOSTNAME)','k0')
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
ifeq ('$(HOSTNAME)','k2')
	cat /boot/grub/menu.lst >         ${HOME}/syscfg/core/boot-grub-menu.lst-k2
	cat /etc/fstab >                  ${HOME}/syscfg/core/etc-fstab-k2
	cat /etc/hosts >                  ${HOME}/syscfg/core/etc-hosts-k2
	cat /etc/inittab >                ${HOME}/syscfg/core/etc-inittab-k2
	cat /etc/makepkg.conf >           ${HOME}/syscfg/core/etc-makepkg.conf-k2
	cat /etc/network.d/k2-wired > ${HOME}/syscfg/core/etc-network.d-k2-wired
	cat /etc/rc.conf >                ${HOME}/syscfg/core/etc-rc.conf-k2
	cat /etc/rc.local >               ${HOME}/syscfg/core/etc-rc.local-k2
	cat /etc/rc.local.shutdown >      ${HOME}/syscfg/core/etc-rc.local.shutdown-k2
	cat /etc/yaourtrc >               ${HOME}/syscfg/core/etc-yaourtrc-k2
	cat /etc/sudoers >                ${HOME}/syscfg/core/etc-sudoers-k2
endif
ifeq ('$(HOSTNAME)','k1')
	cat /boot/grub/menu.lst >         ${HOME}/syscfg/core/boot-grub-menu.lst-k1
	cat /etc/fstab >                  ${HOME}/syscfg/core/etc-fstab-k1
	cat /etc/hosts >                  ${HOME}/syscfg/core/etc-hosts-k1
	cat /etc/makepkg.conf >           ${HOME}/syscfg/core/etc-makepkg.conf-k1
	cat /etc/rc.conf >                ${HOME}/syscfg/core/etc-rc.conf-k1
	cat /etc/rc.local >               ${HOME}/syscfg/core/etc-rc.local-k1
	cat /etc/rc.local.shutdown >      ${HOME}/syscfg/core/etc-rc.local.shutdown-k1
endif
