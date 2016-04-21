C := $(shell pwd)
H := ${HOME}
T := $(shell hostname)
# Add the -n flag for directories, as otherwise, stray symlinks will be created
# inside the C (config) directory itself.
bash:
	ln -fs ${C}/bash/.bashrc                            ${H}
bundle:
	mkdir ${H}/.bundle
	ln -fs ${C}/bundle/cfg                              ${H}/.bundle/config
cron:
	${C}/cron/setcron.sh
emacs:
	ln -fns ${C}/emacs                                  ${H}/.emacs.d
git:
	ln -fs ${C}/git/cfg                                 ${H}/.gitconfig
	ln -fs ${C}/git/template                            ${H}/.git-templates
	ln -fs ${C}/git/sendemail-aliases                   ${H}/.git-sendemail-aliases
gpg:
	ln -fs ${C}/gpg                                     ${H}/.gnupg
gsy:
	ln -fs ${C}/gsy/cfg                                 ${H}/.gsy
gtk:
	ln -fs ${C}/gtk/cfg                                 ${H}/.gtkrc-2.0.mine
lesskey:
	ln -fs ${C}/lesskey/cfg                             ${H}/.lesskey
	lesskey
mplayer:
	ln -fns ${C}/mplayer                                ${H}/.mplayer
mpv:
	ln -fns ${C}/mpv ${H}/.config/mpv
mupen:
	ln -fs ${C}/mupen/InputAutoCfg.ini                  /usr/share/mupen64plus
mutt:
	ln -fns ${C}/mutt                                   ${H}/.mutt
	ln -fs ${C}/mutt/cfg                                ${H}/.muttrc
	ln -fs ${C}/mutt/mailcap                            ${H}/.mailcap
nixos:
	ln -fs ${C}/nixos/${T}/configuration.nix            /etc/nixos
	ln -fs ${C}/nixos/${T}/hardware-configuration.nix   /etc/nixos
nixpkgs:
	ln -fns ${C}/nixpkgs                                ${H}/.nixpkgs
pentadactyl:
	ln -fs ${C}/pentadactyl/cfg                         ${H}/.pentadactylrc
pulse:
	rm -rf ${H}/.config/pulse
	rm -rf ${H}/.pulse
	mkdir -p ${H}/.config/pulse
	mkdir ${H}/.pulse
ifeq ('${T}','k0')
	cp ${C}/pulse/daemon.conf.k0                        ${H}/.config/pulse/daemon.conf
	cp ${C}/pulse/default.pa.k0                         ${H}/.pulse/default.pa
else
	cp ${C}/pulse/daemon.conf.w0                        ${H}/.config/pulse/daemon.conf
	cp ${C}/pulse/default.pa                            ${H}/.pulse/default.pa
endif
qutebrowser:
	ln -fns ${C}/qutebrowser                            ${H}/.config
rf:
	ln -fs ${C}/rf/cfg                                  ${H}/.rf
rtorrent:
	ln -fs ${C}/rtorrent/cfg                            ${H}/.rtorrent.rc
tig:
	ln -fs ${C}/tig/.tigrc                              ${H}
usbmnt:
	ln -fs ${C}/usbmnt/cfg                              ${H}/.usbmnt
uim:
	ln -fns ${C}/uim                                    ${H}/.uim.d
vim:
	ln -fns ${C}/vim                                    ${H}/.vim
	ln -fs ${C}/vim/cfg                                 ${H}/.vimrc
	ln -fs ${C}/vim/cfg                                 ${H}/.gvimrc
xdefaults:
	ln -fs ${C}/xdefaults/cfg                           ${H}/.Xdefaults
xinitrc:
	ln -fs ${C}/xinitrc/cfg                             ${H}/.xinitrc
xmonad:
	ln -fns ${C}/xmonad                                 ${H}/.xmonad
xsession:
	ln -fs ${C}/xsession/cfg                            ${H}/.xsession
zsh:
	ln -fns ${C}/zsh                                    ${H}/.zsh
	ln -fs ${C}/zsh/.zshrc                              ${H}
	mkdir -p ${H}/.zsh-untracked
urxvt:
	ln -fns ${C}/urxvt                                  ${H}/.urxvt
	ln -fs ${H}/.nix-profile/lib/urxvt/perl             ${H}/.urxvt/ext
