C := $(shell pwd)
H := ${HOME}
S := $(shell uname)
T := $(shell hostname | cut -d. -f1)
# Add the -n flag for directories, as otherwise, stray symlinks will be created
# inside the C (config) directory itself.
alacritty:
	ln -fns ${C}/alacritty                              ${H}/.config/alacritty
ifeq ('${T}','k1')
	ln -fs ${C}/alacritty/alacritty.${T}.yml            ${H}/.config/alacritty/alacritty.yml
else ifeq ('${S}','Linux')
	ln -fs ${C}/alacritty/alacritty.linux.yml           ${H}/.config/alacritty/alacritty.yml
else
	ln -fs ${C}/alacritty/alacritty.mac.yml             ${H}/.config/alacritty/alacritty.yml
endif
bash:
	ln -fs ${C}/bash/.bashrc                            ${H}
cmus:
	ln -fns ${C}/cmus                                   ${H}/.cmus
emacs:
	ln -fns ${C}/emacs                                  ${H}/.emacs.d
	touch                                               ${H}/.emacs.d/custom.el
	xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
	ln -fs ${C}/emacs/org-protocol.desktop              ${H}/.local/share/applications
git:
	ln -fs ${C}/git/cfg.personal.conf                   ${H}/.gitconfig
	ln -fns ${C}/git/template                           ${H}/.git-templates
	ln -fs ${C}/git/sendemail-aliases                   ${H}/.git-sendemail-aliases
gpg:
	ln -fns ${C}/gpg                                    ${H}/.gnupg
	ln -fs ${C}/gpg/gpg-agent.nixos.conf                ${H}/.gnupg/gpg-agent.conf
gtk:
	ln -fs ${C}/gtk/cfg                                 ${H}/.gtkrc-2.0.mine
launch-my-browser:
	ln -fs ${C}/script/launch-my-browser                ${H}/bin
lesskey:
	ln -fs ${C}/lesskey/cfg                             ${H}/.lesskey
	lesskey
mpd:
	ln -fns ${C}/mpd                                    ${H}/.config/mpd
mpv:
	ln -fns ${C}/mpv                                    ${H}/.config/mpv
ifeq ('${T}','k0')
	ln -fs ${C}/mpv/mpv.${T}.conf                       ${H}/.config/mpv/mpv.conf
else
	ln -fs ${C}/mpv/mpv.linux.conf                      ${H}/.config/mpv/mpv.conf
endif
mupen:
	ln -fs ${C}/mupen/InputAutoCfg.ini                  /usr/share/mupen64plus
nixos:
	ln -fs ${C}/nixos/${T}/configuration.nix            /etc/nixos
	ln -fs ${C}/nixos/${T}/hardware-configuration.nix   /etc/nixos
nixpkgs:
	ln -fns ${C}/nixpkgs                                ${H}/.nixpkgs
notmuch:
	ln -fs ${C}/notmuch/notmuch-config                  ${H}/.notmuch-config
nvim:
	ln -fns ${C}/nvim                                   ${H}/.config/nvim
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
	cp ${C}/pulse/default.pa                            ${H}/.pulse/default.pa
endif
qutebrowser:
	ln -fns ${C}/qutebrowser                            ${H}/.config
rtorrent:
	ln -fs ${C}/rtorrent/cfg                            ${H}/.rtorrent.rc
ssh:
ifeq ($(wildcard ~/.ssh/.),)
	ln -fns ${C}/ssh                                    ${H}/.ssh
endif
	ln -fs ${C}/ssh/config.home.conf                    ${H}/.ssh/config
tig:
	ln -fs ${C}/tig/.tigrc                              ${H}
tmux:
	ln -fs ${C}/tmux/.tmux.conf                         ${H}
uim:
	ln -fns ${C}/uim/.uim                               ${H}/.uim
	ln -fns ${C}/uim                                    ${H}/.uim.d
vim:
	ln -fns ${C}/vim                                    ${H}/.vim
	ln -fs ${C}/vim/cfg                                 ${H}/.vimrc
	ln -fs ${C}/vim/cfg                                 ${H}/.gvimrc
xdefaults:
	ln -fs ${C}/xdefaults/cfg                           ${H}/.Xdefaults
xmonad:
	ln -fns ${C}/xmonad                                 ${H}/.xmonad
	# xsession is relied on by NixOS hosts that use xmonad (basically the
	# default) to start xmonad.
	ln -fs ${C}/xmonad/.xsession                        ${H}/.xsession
zathura:
	ln -fns ${C}/zathura                                ${H}/.config/zathura
zsh:
	ln -fns ${C}/zsh                                    ${H}/.zsh
	ln -fs ${C}/zsh/.zshenv                             ${H}
	ln -fs ${C}/zsh/.zshrc                              ${H}
	test -f ${C}/zsh/zprofile-${T} \
		&& ln -fs ${C}/zsh/zprofile-${T}                ${H}/.zprofile
	test -f ${C}/zsh/zlogin-${T} \
		&& ln -fs ${C}/zsh/zlogin-${T}                  ${H}/.zlogin
	mkdir -p ${H}/.zsh-untracked

.PHONY: \
	alacritty \
	bash \
	cmus \
	emacs \
	git \
	gpg \
	gtk \
	lesskey \
	mpd \
	mpv \
	mupen \
	nixos \
	nixpkgs \
	notmuch \
	nvim \
	offlineimap \
	pulse \
	qutebrowser \
	rtorrent \
	ssh \
	tig \
	tmux \
	uim \
	vim \
	xdefaults \
	xmonad \
	xorg \
	zathura \
	zsh
