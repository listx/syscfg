CFGROOT := $(shell pwd)
HOSTNAME := $(shell hostname)
# add the -n flag for directories, as otherwise, stray symlinks will be created inside the CFGROOT directory itself
all: boxes galculator git mpd mplayer ncmpcpp rtorrent shellscripts vim vimperatorrc xdefaults xinitrc xmonad zsh
boxes:
	ln -fs $(CFGROOT)/boxes/cfg         ${HOME}/.boxes
git:
	ln -fs $(CFGROOT)/git/cfg           ${HOME}/.gitconfig
galculator:
	ln -fs $(CFGROOT)/galculator/cfg    ${HOME}/.galculator
mplayer:
	ln -fns $(CFGROOT)/mplayer          ${HOME}/.mplayer
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
rtorrent:
	ln -fs $(CFGROOT)/rtorrent/cfg      ${HOME}/.rtorrent.rc
shellscripts:
	ln -fns $(CFGROOT)/shellscripts     ${HOME}/shellscripts
vim:
	ln -fns $(CFGROOT)/vim              ${HOME}/.vim
	ln -fs $(CFGROOT)/vim/cfg           ${HOME}/.vimrc
	ln -fs $(CFGROOT)/vim/cfg-gui       ${HOME}/.gvimrc
vimperatorrc:
	ln -fs $(CFGROOT)/vimperatorrc/cfg  ${HOME}/.vimperatorrc
xdefaults:
	ln -fs $(CFGROOT)/xdefaults/cfg     ${HOME}/.Xdefaults
xinitrc:
	ln -fs $(CFGROOT)/xinitrc/cfg       ${HOME}/.xinitrc
xmonad:
	ln -fns $(CFGROOT)/xmonad           ${HOME}/.xmonad
	ln -fs $(CFGROOT)/xmonad/init       ${HOME}/.xmonad/init.sh
#--------------------------------------------------------------------------------------------------#
# Since it's really painful to do a unified config file across multiple hosts in XMonad v. 0.8.1,  #
# I have to do it this way.                                                                        #
# NOTE: As of March 2009, the ifeq and its sibling statements MUST START at the BEGINNING of the   #
# line!                                                                                            #
#--------------------------------------------------------------------------------------------------#
ifeq ('$(HOSTNAME)','exelion')
	ln -fs $(CFGROOT)/xmonad/cfg        ${HOME}/.xmonad/xmonad.hs
endif
ifeq ('$(HOSTNAME)','luxion')
	ln -fs $(CFGROOT)/xmonad/cfg-luxion ${HOME}/.xmonad/xmonad.hs
endif
ifeq ('$(HOSTNAME)','aether')
	ln -fs $(CFGROOT)/xmonad/cfg-aether ${HOME}/.xmonad/xmonad.hs
endif

zsh:
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
	rm ${HOME}/.vim
	rm ${HOME}/.vimrc
	rm ${HOME}/.gvimrc
	rm ${HOME}/shellscripts
	rm ${HOME}/.vimperatorrc
	rm ${HOME}/.Xdefaults
	rm ${HOME}/.xinitrc
	rm ${HOME}/.xmonad/init.sh
	rm ${HOME}/.xmonad/xmonad.hs
	rm ${HOME}/.xmonad
	rm ${HOME}/.zsh
	rm ${HOME}/.zshrc

# copy contents of system files to keep track of them
syscopy:
ifeq ('$(HOSTNAME)','exelion')
	cat /boot/grub/menu.lst >       /home/listdata/syscfg/sys/boot-grub-menu.lst-exelion
	cat /etc/X11/xorg.conf >        /home/listdata/syscfg/sys/etc-X11-xorg.conf-exelion
	cat /etc/fstab >                /home/listdata/syscfg/sys/etc-fstab-exelion
	cat /etc/hosts >                /home/listdata/syscfg/sys/etc-hosts-exelion
	cat /etc/inittab >              /home/listdata/syscfg/sys/etc-inittab-exelion
	cat /etc/makepkg.conf >         /home/listdata/syscfg/sys/etc-makepkg.conf-exelion
	cat /etc/rc.conf >              /home/listdata/syscfg/sys/etc-rc.conf-exelion
	cat /etc/rc.local >             /home/listdata/syscfg/sys/etc-rc.local-exelion
	cat /etc/rc.local.shutdown >    /home/listdata/syscfg/sys/etc-rc.local.shutdown-exelion
	cat /etc/yaourtrc >             /home/listdata/syscfg/sys/etc-yaourtrc-exelion
	cat /etc/sudoers >              /home/listdata/syscfg/sys/etc-sudoers-exelion # requires superuser privileges to read!
endif
ifeq ('$(HOSTNAME)','luxion')
	cat /boot/grub/menu.lst >       /home/listdata/syscfg/sys/boot-grub-menu.lst-luxion
	cat /etc/X11/xorg.conf >        /home/listdata/syscfg/sys/etc-X11-xorg.conf-luxion
	cat /etc/fstab >                /home/listdata/syscfg/sys/etc-fstab-luxion
	cat /etc/hosts >                /home/listdata/syscfg/sys/etc-hosts-luxion
	cat /etc/inittab >              /home/listdata/syscfg/sys/etc-inittab-luxion
	cat /etc/makepkg.conf >         /home/listdata/syscfg/sys/etc-makepkg.conf-luxion
	cat /etc/network.d/luxion-wired > /home/listdata/syscfg/sys/etc-network.d-luxion-wired
	cat /etc/rc.conf >              /home/listdata/syscfg/sys/etc-rc.conf-luxion
	cat /etc/rc.local >             /home/listdata/syscfg/sys/etc-rc.local-luxion
	cat /etc/rc.local.shutdown >    /home/listdata/syscfg/sys/etc-rc.local.shutdown-luxion
	cat /etc/yaourtrc >             /home/listdata/syscfg/sys/etc-yaourtrc-luxion
	cat /etc/sudoers >              /home/listdata/syscfg/sys/etc-sudoers-luxion
endif
ifeq ('$(HOSTNAME)','aether')
	cat /boot/grub/menu.lst >       /home/listdata/syscfg/sys/boot-grub-menu.lst-aether
	cat /etc/X11/xorg.conf >        /home/listdata/syscfg/sys/etc-X11-xorg.conf-aether
	cat /etc/fstab >                /home/listdata/syscfg/sys/etc-fstab-aether
	cat /etc/hosts >                /home/listdata/syscfg/sys/etc-hosts-aether
	cat /etc/makepkg.conf >         /home/listdata/syscfg/sys/etc-makepkg.conf-aether
	cat /etc/rc.conf >              /home/listdata/syscfg/sys/etc-rc.conf-aether
	cat /etc/rc.local >             /home/listdata/syscfg/sys/etc-rc.local-aether
endif
