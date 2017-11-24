C := $(shell pwd)
H := ${HOME}
S := $(shell uname)
T := $(shell hostname)
# Add the -n flag for directories, as otherwise, stray symlinks will be created
# inside the C (config) directory itself.
bash:
	ln -fs ${C}/bash/.bashrc                            ${H}
emacs:
	ln -fns ${C}/emacs                                  ${H}/.emacs.d
	touch                                               ${H}/.emacs.d/custom.el
git:
ifeq ('${S}','Darwin')
	ln -fs ${C}/git/cfg.google.conf                     ${H}/.gitconfig
else
	ln -fs ${C}/git/cfg.personal.conf                   ${H}/.gitconfig
endif
	ln -fns ${C}/git/template                           ${H}/.git-templates
	ln -fs ${C}/git/sendemail-aliases                   ${H}/.git-sendemail-aliases
gpg:
	ln -fns ${C}/gpg                                    ${H}/.gnupg
ifeq ('${S}','Darwin')
	ln -fs ${C}/gpg/gpg-agent.ubuntu.conf               ${H}/.gnupg/gpg-agent.conf
else
	ln -fs ${C}/gpg/gpg-agent.nixos.conf                ${H}/.gnupg/gpg-agent.conf
endif
gtk:
	ln -fs ${C}/gtk/cfg                                 ${H}/.gtkrc-2.0.mine
lesskey:
	ln -fs ${C}/lesskey/cfg                             ${H}/.lesskey
	lesskey
mpv:
	ln -fns ${C}/mpv                                    ${H}/.config/mpv
ifeq ('${T}','k0')
	ln -fs ${C}/mpv/mpv.${T}.conf                       ${H}/.config/mpv/mpv.conf
else
	ln -fs ${C}/mpv/mpv.linux.conf                      ${H}/.config/mpv/mpv.conf
endif
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
notmuch:
	ln -fs ${C}/notmuch/notmuch-config                  ${H}/.notmuch-config
offlineimap:
	ln -fs ${C}/offlineimap/offlineimaprc               ${H}/.offlineimaprc
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
rtorrent:
	ln -fs ${C}/rtorrent/cfg                            ${H}/.rtorrent.rc
sage:
	ln -fns ${C}/sage                                   ${H}/.sage
ssh:
	ln -fns ${C}/ssh                                    ${H}/.ssh
ifeq ('${T}','Darwin')
	ln -fs ${C}/ssh/config.google.conf                  ${H}/.ssh/config
else
	ln -fs ${C}/ssh/config.home.conf                    ${H}/.ssh/config
endif
tig:
	ln -fs ${C}/tig/.tigrc                              ${H}
uim:
	ln -fns ${C}/uim/.uim                               ${H}/.uim
	ln -fns ${C}/uim                                    ${H}/.uim.d
urxvt:
	ln -fns ${C}/urxvt                                  ${H}/.urxvt
ifeq ('${S}','Darwin')
	ln -fns ${H}/prog/foreign/urxvt-perls               ${H}/.urxvt/ext
	# NOTE: These two plugins are deprecated, but we use them anyway because we
	# are too lazy to fix it the "right" way.
	ln -fs ${H}/prog/foreign/urxvt-perls/deprecated/clipboard  ${H}/.urxvt/ext
	ln -fs ${H}/prog/foreign/urxvt-perls/deprecated/url-select ${H}/.urxvt/ext
	ln -fs ${H}/prog/foreign/urxvt-font-size/font-size  ${H}/.urxvt/ext
else
	ln -fns ${H}/.nix-profile/lib/urxvt/perl            ${H}/.urxvt/ext
endif
vim:
	ln -fns ${C}/vim                                    ${H}/.vim
	ln -fs ${C}/vim/cfg                                 ${H}/.vimrc
	ln -fs ${C}/vim/cfg                                 ${H}/.gvimrc
xdefaults:
	ln -fs ${C}/xdefaults/cfg                           ${H}/.Xdefaults
xmonad:
	ln -fns ${C}/xmonad                                 ${H}/.xmonad
ifneq (,$(findstring enif,${T}))
	ln -fns ${C}/xmonad/ubuntu-xmonad-startup.sh        ${H}/.xmonad/xmonad-session-rc
endif
zsh:
	ln -fns ${C}/zsh                                    ${H}/.zsh
	ln -fs ${C}/zsh/.zshrc                              ${H}
	mkdir -p ${H}/.zsh-untracked
ifeq ('${S}','Darwin')
	ln -fs ${C}/zsh/zprofile-mac                        ${H}/.zprofile
endif
