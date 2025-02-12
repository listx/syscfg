# Configuration directory.
C := $(shell pwd)
# Home directory.
H := ${HOME}
# Platform. E.g., "Darwin", "Linux".
P := $(shell uname)
# Long hostname, without the trailing domain name, if any.
T := $(shell hostname | cut -d. -f1)
# Short (custom) hostname.
S := $(shell cat ${H}/.hostname-short)

# Add the -n flag for directories, as otherwise, stray symlinks will be created
# inside the C (config) directory itself.

bash:
	ln -fs ${C}/bash/.bashrc                            ${H}
clojure:
	ln -fns ${C}/clojure                                ${H}/.config/clojure
emacs:
	git -C ${C} submodule update --init ${C}/emacs/doom-upstream
	# Point .emacs.d to upstream doom code.
	ln -fns ${C}/emacs/doom-upstream                    ${H}/.emacs.d
	# Make $$DOOMDIR point to our doom-cfg folder.
	ln -fns ${C}/emacs/doom-cfg                         ${H}/.doom.d
	# Bring 'doom' script into $$PATH.
	cd ${C}/script && ln -fs ../emacs/doom-upstream/bin/doom
	ln -sf ${H}/lo/custom-dict.txt                      ${C}/emacs/spell-fu
git:
	ln -fs ${C}/git/cfg.personal.conf                   ${H}/.gitconfig
	ln -fs ${C}/git/gitignore                           ${H}/.gitignore
	ln -fs ${C}/git/sendemail-aliases                   ${H}/.git-sendemail-aliases
gpg:
	ln -fns ${C}/gpg                                    ${H}/.gnupg
	ln -fs ${C}/gpg/gpg-agent.nixos.conf                ${H}/.gnupg/gpg-agent.conf
karabiner:
	mkdir -p ${H}/.config/karabiner
	ln -fns ${C}/karabiner/karabiner.json               ${H}/.config/karabiner
lesskey:
	ln -fs ${C}/lesskey/cfg                             ${H}/.lesskey
	lesskey
melby:
	ln -fns ${C}/melby                                   ${H}/.melby
	set -e && \
		melby_location=$$(readlink $$(which melbyd)) && \
		cp -f $${melby_location%/bin/melbyd}/share/melby/sample/* ${H}/.melby
mpd:
	ln -fns ${C}/mpd                                    ${H}/.config/mpd
mpv:
	ln -fns ${C}/mpv                                    ${H}/.config/mpv
ifeq ('${T}','k0')
	ln -fs mpv.${T}.conf                                mpv/mpv.conf
else ifeq ('${P}','Darwin')
	ln -fs mpv.osx.conf                                 mpv/mpv.conf
else
	ln -fs mpv.linux.conf                               mpv/mpv.conf
endif
nix:
	ln -fns ${C}/nix                                    ${H}/.config/nix
nixos:
ifeq ('${T}','w0')
	ln -fs ${C}/nixos/${T}/configuration.nix            /etc/nixos
	ln -fs ${C}/nixos/${T}/syschdemd.nix                /etc/nixos
	ln -fs ${C}/nixos/${T}/syschdemd.sh                 /etc/nixos
	ln -fs ${C}/nixos/${T}/wsl.conf                     /etc/nixos
	ln -fs ${C}/nixos/${T}/hosts                        /etc
	cp -f ${C}/nixos/${T}/resolv.conf                   /etc
else
	ln -fs ${C}/nixos/${T}/configuration.nix            /etc/nixos
	ln -fs ${C}/nixos/${T}/hardware-configuration.nix   /etc/nixos
endif
nixpkgs:
	ln -fns ${C}/nixpkgs                                ${H}/.nixpkgs
notmuch:
	ln -fs ${C}/notmuch/notmuch-config                  ${H}/.notmuch-config
nvim:
	ln -fns ${C}/nvim                                   ${H}/.config/nvim
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
ifeq ('${P}','Darwin')
	ln -fns ${C}/qutebrowser                            ${H}/.qutebrowser
else
	ln -fns ${C}/qutebrowser                            ${H}/.config
endif
rtorrent:
	ln -fs ${C}/rtorrent/cfg                            ${H}/.rtorrent.rc
ssh:
ifeq ($(wildcard ~/.ssh/.),)
	ln -fns ${C}/ssh                                    ${H}/.ssh
endif
	ln -fs ${C}/ssh/config.home.conf                    ${H}/.ssh/config
terminfo:
	git -C ${C} submodule update --init ${C}/wezterm/upstream
	tic -x -o ~/.terminfo ${C}/wezterm/upstream/termwiz/data/wezterm.terminfo
	tic -x -o ~/.terminfo ${C}/terminfo/xterm-24bit.terminfo
tig:
	ln -fs ${C}/tig/.tigrc                              ${H}
tmux:
	ln -fs ${C}/tmux/.tmux.conf                         ${H}/.tmux.conf
	ln -fns ${C}/tmux                                   ${H}/.tmux
uim:
	ln -fns ${C}/uim/.uim                               ${H}/.uim
	ln -fns ${C}/uim                                    ${H}/.uim.d
vim:
	ln -fns ${C}/vim                                    ${H}/.vim
	ln -fs ${C}/vim/cfg                                 ${H}/.vimrc
	ln -fs ${C}/vim/cfg                                 ${H}/.gvimrc
vimpc:
	ln -fs ${C}/vimpc/.vimpcrc                          ${H}
wezterm:
	ln -fs ${C}/wezterm/.wezterm.lua                    ${H}
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
		&& ln -fs ${C}/zsh/zprofile-${T}                ${H}/.zprofile || true
	test -f ${C}/zsh/zprofile-${S} \
		&& ln -fs ${C}/zsh/zprofile-${S}                ${H}/.zprofile || true
	test -f ${C}/zsh/zlogin-${T} \
		&& ln -fs ${C}/zsh/zlogin-${T}                  ${H}/.zlogin || true
	mkdir -p ${H}/.zsh-untracked
	git -C ${C} submodule update --init ${C}/zsh/zcomet-upstream
	mkdir -p ${C}/zsh/zcomet
	ln -fns ${C}/zsh/zcomet                             ${H}/.zcomet

.PHONY: \
	bash \
	clojure \
	emacs \
	git \
	gpg \
	karabiner \
	lesskey \
	melby \
	mpd \
	mpv \
	nix \
	nixos \
	nixpkgs \
	notmuch \
	nvim \
	pulse \
	qutebrowser \
	rtorrent \
	ssh \
	terminfo \
	tig \
	tmux \
	uim \
	vim \
	vimpc \
	wezterm \
	xmonad \
	xorg \
	zathura \
	zsh
