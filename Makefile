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
ifeq ('$(HOSTNAME)','exelion')
	ln -fs $(CFGROOT)/ncmpcpp/cfg       ${HOME}/.ncmpcpp/config
endif
ifeq ('$(HOSTNAME)','luxion')
	ln -fs $(CFGROOT)/ncmpcpp/cfg-luxion ${HOME}/.ncmpcpp/config
endif
ifeq ('$(HOSTNAME)','aether')
	ln -fs $(CFGROOT)/ncmpcpp/cfg-luxion ${HOME}/.ncmpcpp/config
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
ifeq ('$(HOSTNAME)','exelion')
	ln -fs $(CFGROOT)/rtorrent/cfg       ${HOME}/.rtorrent.rc
endif
ifeq ('$(HOSTNAME)','aether')
	ln -fs $(CFGROOT)/rtorrent/cfg-aether ${HOME}/.rtorrent.rc
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
ifeq ('$(HOSTNAME)','exelion')
	ln -fs $(CFGROOT)/xmonad/others.hs           ${HOME}/.xmonad/xmonad.hs
endif
ifeq ('$(HOSTNAME)','luxion')
	ln -fs $(CFGROOT)/xmonad/luxion.hs           ${HOME}/.xmonad/xmonad.hs
endif
ifeq ('$(HOSTNAME)','aether')
	ln -fs $(CFGROOT)/xmonad/others.hs           ${HOME}/.xmonad/xmonad.hs
endif
xorg:
	ln -fns $(CFGROOT)/xorg/10-keyboard.conf	/etc/X11/xorg.conf.d/
	ln -fns $(CFGROOT)/xorg/10-server-flags.conf	/etc/X11/xorg.conf.d/
ifeq ('$(HOSTNAME)','exelion')
	ln -fns $(CFGROOT)/xorg/10-dual-monitor-hybrid.conf		/etc/X11/xorg.conf.d/
endif
ifeq ('$(HOSTNAME)','luxion')
	ln -fns $(CFGROOT)/xorg/10-synaptics.conf	/etc/X11/xorg.conf.d/
endif
ifeq ('$(HOSTNAME)','aether')
	ln -fns $(CFGROOT)/xorg/10-synaptics.conf	/etc/X11/xorg.conf.d/
endif
zsh:
	mkdir ${HOME}/.zsh-untracked
	ln -fns $(CFGROOT)/zsh              ${HOME}/.zsh
	ln -fs $(CFGROOT)/zsh/cfg           ${HOME}/.zshrc

uninstall:
	rm ${HOME}/.boxes
	rm ${HOME}/.gitconfig
ifeq ('$(HOSTNAME)','exelion')
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
ifeq ('$(HOSTNAME)','exelion')
	cat /boot/grub/menu.lst >         /home/listdata/syscfg/core/boot-grub-menu.lst-exelion
	cat /etc/fstab >                  /home/listdata/syscfg/core/etc-fstab-exelion
	cat /etc/hosts >                  /home/listdata/syscfg/core/etc-hosts-exelion
	cat /etc/inittab >                /home/listdata/syscfg/core/etc-inittab-exelion
	cat /etc/makepkg.conf >           /home/listdata/syscfg/core/etc-makepkg.conf-exelion
	cat /etc/rc.conf >                /home/listdata/syscfg/core/etc-rc.conf-exelion
	cat /etc/rc.local >               /home/listdata/syscfg/core/etc-rc.local-exelion
	cat /etc/rc.local.shutdown >      /home/listdata/syscfg/core/etc-rc.local.shutdown-exelion
	cat /etc/yaourtrc >               /home/listdata/syscfg/core/etc-yaourtrc-exelion
	cat /etc/sudoers >                /home/listdata/syscfg/core/etc-sudoers-exelion # requires superuser privileges to read!
endif
ifeq ('$(HOSTNAME)','luxion')
	cat /boot/grub/menu.lst >         /home/listdata/syscfg/core/boot-grub-menu.lst-luxion
	cat /etc/fstab >                  /home/listdata/syscfg/core/etc-fstab-luxion
	cat /etc/hosts >                  /home/listdata/syscfg/core/etc-hosts-luxion
	cat /etc/inittab >                /home/listdata/syscfg/core/etc-inittab-luxion
	cat /etc/makepkg.conf >           /home/listdata/syscfg/core/etc-makepkg.conf-luxion
	cat /etc/network.d/luxion-wired > /home/listdata/syscfg/core/etc-network.d-luxion-wired
	cat /etc/rc.conf >                /home/listdata/syscfg/core/etc-rc.conf-luxion
	cat /etc/rc.local >               /home/listdata/syscfg/core/etc-rc.local-luxion
	cat /etc/rc.local.shutdown >      /home/listdata/syscfg/core/etc-rc.local.shutdown-luxion
	cat /etc/yaourtrc >               /home/listdata/syscfg/core/etc-yaourtrc-luxion
	cat /etc/sudoers >                /home/listdata/syscfg/core/etc-sudoers-luxion
endif
ifeq ('$(HOSTNAME)','aether')
	cat /boot/grub/menu.lst >         /home/listdata/syscfg/core/boot-grub-menu.lst-aether
	cat /etc/fstab >                  /home/listdata/syscfg/core/etc-fstab-aether
	cat /etc/hosts >                  /home/listdata/syscfg/core/etc-hosts-aether
	cat /etc/makepkg.conf >           /home/listdata/syscfg/core/etc-makepkg.conf-aether
	cat /etc/rc.conf >                /home/listdata/syscfg/core/etc-rc.conf-aether
	cat /etc/rc.local >               /home/listdata/syscfg/core/etc-rc.local-aether
	cat /etc/rc.local.shutdown >      /home/listdata/syscfg/core/etc-rc.local.shutdown-aether
endif
