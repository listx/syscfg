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

alacritty:
	ln -fns ${C}/alacritty                              ${H}/.config/alacritty
	ln -fs colors_PastelDark.yml                        alacritty/colors.yml
ifeq ('${T}','k1')
	ln -fs k1_tmux.yml                                  alacritty/alacritty.yml
	ln -fs k1_shell.yml                                 alacritty/alacritty_raw_shell.yml
else ifeq ('${T}','m0')
	ln -fs k1_tmux.yml                                  alacritty/alacritty.yml
	ln -fs k1_shell.yml                                 alacritty/alacritty_raw_shell.yml
else ifeq ('${P}','Linux')
	ln -fs linux_tmux.yml                               alacritty/alacritty.yml
	ln -fs linux_shell.yml                              alacritty/alacritty_raw_shell.yml
else
	ln -fs mac_tmux.yml                                 alacritty/alacritty.yml
	ln -fs mac_shell.yml                                alacritty/alacritty_raw_shell.yml
endif
bash:
	ln -fs ${C}/bash/.bashrc                            ${H}
cmus:
	ln -fns ${C}/cmus                                   ${H}/.cmus
emacs:
	git -C ${C} submodule update --init ${C}/emacs/doom-upstream
	# Point .emacs.d to upstream doom code.
	ln -fns ${C}/emacs/doom-upstream                    ${H}/.emacs.d
	# Make $$DOOMDIR point to our doom-cfg folder.
	ln -fns ${C}/emacs/doom-cfg                         ${H}/.doom.d
	# Bring 'doom' script into $$PATH.
	cd ${C}/script && ln -fs ../emacs/doom-upstream/bin/doom
git:
	ln -fs ${C}/git/cfg.personal.conf                   ${H}/.gitconfig
	ln -fns ${C}/git/template                           ${H}/.git-templates
	ln -fs ${C}/git/sendemail-aliases                   ${H}/.git-sendemail-aliases
gpg:
	ln -fns ${C}/gpg                                    ${H}/.gnupg
	ln -fs ${C}/gpg/gpg-agent.nixos.conf                ${H}/.gnupg/gpg-agent.conf
gtk:
	ln -fs ${C}/gtk/cfg                                 ${H}/.gtkrc-2.0.mine
karabiner:
	mkdir -p ${H}/.config/karabiner
	ln -fns ${C}/karabiner/karabiner.json               ${H}/.config/karabiner
kube:
	mkdir -p ${C}/kube
	cp -f  /etc/kubernetes/cluster-admin.kubeconfig     ${C}/kube/config
	ln -fns ${C}/kube                                   ${H}/.kube
	# Give normal user cluster-admin rights. This is required for invoking
	# kubectl against the local cluster. It is by default set to root
	# read-access only "so that you cannot gain cluster-admin rights just by
	# being a normal user (by default)", according to
	# https://logs.nix.samueldr.com/nixos-kubernetes/2018-09-07.
	sudo chmod go+r /var/lib/kubernetes/secrets/cluster-admin-key.pem
lesskey:
	ln -fs ${C}/lesskey/cfg                             ${H}/.lesskey
	lesskey
lhc:
	make -C ${C}/lhc
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
mupen:
	ln -fs ${C}/mupen/InputAutoCfg.ini                  /usr/share/mupen64plus
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
	ln -fns ${C}/nixpkgs                                ${H}/.config/nixpkgs
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
terminfo:
	git -C ${C} submodule update --init ${C}/alacritty/upstream
	tic -x -o ~/.terminfo ${C}/alacritty/upstream/extra/alacritty.info
	tic -x -o ~/.terminfo ${C}/terminfo/xterm-24bit.terminfo
	tic -x -o ~/.terminfo ${C}/terminfo/alacritty-xtermlike.terminfo
tig:
	ln -fs ${C}/tig/.tigrc                              ${H}
tmux:
	ln -fs ${C}/tmux/.tmux.conf                         ${H}
	git -C ${C} submodule update --init ${C}/tmux/plugins
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
		&& ln -fs ${C}/zsh/zprofile-${T}                ${H}/.zprofile || true
	test -f ${C}/zsh/zprofile-${S} \
		&& ln -fs ${C}/zsh/zprofile-${S}                ${H}/.zprofile || true
	test -f ${C}/zsh/zlogin-${T} \
		&& ln -fs ${C}/zsh/zlogin-${T}                  ${H}/.zlogin || true
	mkdir -p ${H}/.zsh-untracked
	git -C ${C} submodule update --init ${C}/zsh/zcomet-upstream
	ln -fns ${C}/zsh/zcomet                             ${H}/.zcomet

.PHONY: \
	alacritty \
	bash \
	cmus \
	emacs \
	git \
	gpg \
	gtk \
	karabiner \
	kube \
	lesskey \
	lhc \
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
	terminfo \
	tig \
	tmux \
	uim \
	vim \
	vimpc \
	wezterm \
	xdefaults \
	xmonad \
	xorg \
	zathura \
	zsh
