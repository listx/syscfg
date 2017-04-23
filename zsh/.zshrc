# Environment variables

# Set 'ls' command's color theme
# See "dircolors -p" for more info
#
# Below are the color init strings for the basic file types. A color init
# string consists of one or more of the following numeric codes:
# Attribute codes:
# 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
# Text color codes:
# 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
# Background color codes:
# 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white

# rs=RESET (reset to normal color)
# di=DIR
# ln=LINK (symlink)
# mh=MULTIHARDLINK
# pi=FIFO (pipe)
# so=SOCK
# do=DOOR
# bd=BLK (block device driver)
# cd=CHR
# or=ORPHAN (symlink to nonexistent file)
# su=SETUID (file that is setuid (u+s))
# sg=SETGID (file that is setgid (g+s))
# ca=CAPABILITY (file with capability)
# tw=STICKY_OTHER_WRITABLE (dir that is sticky and other-writable (+t,o+w))
# ow=OTHER_WRITABLE (dir that is other-writable (o+w) and not sticky
# st=STICKY (dir wit hthe sticky bit set (+t) and not other-writable)
# ex=EXEC (files with +x execute permission)

# special file types (same, unchanged defaults from 'dircolors -b')
LS_COLORS='rs=0:di=01;33:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:st=37;44:'
# special file types (different than the ones from 'dircolors -b')
LS_COLORS+='ln=36:ex=31:tw=1;34;42:ow=1;7;34:'
# various common filetypes (archives, pictures, movies, and sounds)
ft_ark=(tar tgz arj taz lzh lzma tlz txz zip z Z dz gz lz xz bz2 bz tbz tbz2 tz deb rpm jar rar ace zoo cpio 7z rz)
ft_pic=(jpg jpeg gif bmp pbm pgm ppm tga xbm xpm tif tiff png svg svgz mng pcx xcf)
ft_mov=(mov mpg mpeg m2v mkv ogm mp4 m4v mp4v vob qt nuv wmv asf rm rmvb flc avi fli flv gl dl xwd yuv cgm emf axv anx ogv ogx webm)
ft_snd=(aac au flac m4a mid midi mka mp3 mpc ogg ra wav axa oga spx xspf)
for t in $ft_ark; do LS_COLORS+="*.$t=32:"; done
for t in $ft_pic; do LS_COLORS+="*.$t=35:"; done
for t in $ft_mov; do LS_COLORS+="*.$t=1;35:"; done
for t in $ft_snd; do LS_COLORS+="*.$t=34:"; done
# custom file types not covered by 'dircolors -p'
LS_COLORS+="*.toucan=0;38;5;48:"
LS_COLORS+="*.cog=0;38;5;48:"
export LS_COLORS

# colors for the 'less' pager
export LESS_TERMCAP_md=$'\E[1;34m'      # begin bold (blue)
export LESS_TERMCAP_us=$'\E[1;35m'      # begin underline (magenta)
export LESS_TERMCAP_so=$'\E[1;34;5;43m' # begin standout-mode - (search highlight) (blue on yellow)
export LESS_TERMCAP_mb=$'\E[1;5;31m'    # begin blinking (the last "5" actually makes it blink) (red)
export LESS_TERMCAP_me=$'\E[0m'         # end bold/blinking
export LESS_TERMCAP_se=$'\E[0m'         # end standout-mode
export LESS_TERMCAP_ue=$'\E[0m'         # end underline

# default text editor--set it to vim, not vi
export VISUAL=vim
export EDITOR=vim

# wine directory (prevent wine/msiexec from creating a default .wine folder)
export WINEARCH=win32
export WINEPREFIX=~/.win32
# disable wine from creating retarded file associations (e.g., opening .txt files with notepad.exe from Firefox)
export WINEDLLOVERRIDES='winemenubuilder.exe=d'

# haskell AUR packages: enable profiling (-p) option
export PKGBUILD_HASKELL_ENABLE_PROFILING=1

# default ledger file (to avoid using -f option every time)
export LEDGER_FILE=~/org/master.hledger

# make GCC print errors in color
export GCC_COLORS=1

# path to local nixpkgs git repo
export NIXPKGS=~/prog/foreign/nixpkgs

# stop zsh from eating space before pipe symbol
export ZLE_REMOVE_SUFFIX_CHARS=""

autoload -U compinit
compinit

zmodload zsh/complist # for the 'menuselect' keymap

fpath=(~/.zsh/func $fpath) # add ~/.zsh/func to $fpath
autoload -U ~/.zsh/func/*(:t) # load all functions in ~/.zsh/func

autoload -U promptinit
promptinit

autoload -U zmv

# This will set the default prompt to the kody theme
prompt kody

# history settings
HISTSIZE=20000
SAVEHIST=40000
HISTFILE=~/.zsh-untracked/history

# Directory stack size
DIRSTACKSIZE=20

setopt SHARE_HISTORY
setopt EXTENDED_HISTORY # puts timestamps in the history
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE # if a command or alias begins with a space, do not store into history
setopt HIST_SAVE_NO_DUPS

# bring up the completion menu with <tab><tab>, but with just <tab>, behave like the default
setopt auto_menu
# automatically complete an ambiguous word to the nearest, partial-word match
# (combined with auto_menu, this makes it so that you only have to type <tab>
# once, and only once, in all situations)
unsetopt listambiguous

# Make cd push the old directory onto the directory stack.
setopt auto_pushd
# don't list dir stack each time we use cd
setopt pushdsilent
# don't push in duplicate dirs into the dir stack
setopt pushdignoredups

zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} # use the LS_COLORS variable to colorize the completion list
LISTMAX=9999 # don't ask 'do you wish to see all N possibilities' unless N > 9999

# make '*' expand to catch dotfiles as well
setopt GLOB_DOTS

# Custom binds for some common operations. Note, however, that you can enter
# vi-like "normal mode" with CTRL+X, CTRL+V for some more advanced operations.

bindkey -v # use vim bindings!
# use CTRL-R for history search (^R is bound to the 'redisplay' command by default, which is never used anyway)
bindkey     '' history-incremental-search-backward
# fix backspace key breakage on remote server
if [[ "$HOSTNAME" == "l0" ]]; then
    TERM=xterm
fi
bindkey     '^[[3~'         delete-char         # DELETE key
bindkey     '^[[7~'         beginning-of-line   # HOME key
bindkey     '^[[8~'         end-of-line         # END key
bindkey -M menuselect '' .accept-line # just execute the command when selecting from a menu and pressing <enter>
#bindkey     '\t' menu-expand-or-complete   # press <tab> just ONCE to bring up menu *AND* select the first item

# make 'ds' prompt the user to select a dir from the dir stack, instead of just listing the dir stick
# with a plain 'dirs -v'
# NOTE: 'cd +$n' makes it go to the number listed, whereas 'cd -$n' reverses it... don't know why
# you'd ever want to use 'cd -$n'
alias ds=' dirs -v ; echo -n "\nSelect directory: " ; read n ; cd +$n && ll'
# just show a list of dir stack without choose prompt
alias dl=' dirs -v'

# Useful change dirs (first change to the directory, and then list the contents
# within; notice how you can use aliases within aliases! (the "l" is an alias
# here defined below)

alias k=" cd ..;ll"
alias kk=" cd ../../;ll"
alias kkk=" cd ../../../;ll"
alias kkkk=" cd ../../../../;ll"
alias kkkkk=" cd ../../../../../;ll"
alias kkkkkk=" cd ../../../../../../;ll"
alias kkkkkkk=" cd ../../../../../../../;ll"
alias kkkkkkkk=" cd ../../../../../../../../;ll"
alias kkkkkkkkk=" cd ../../../../../../../../../;ll"
alias kkkkkkkkkk=" cd ../../../../../../../../../../;ll"
alias kkkkkkkkkkk=" cd ../../../../../../../../../../../;ll"
alias kkkkkkkkkkkk=" cd ../../../../../../../../../../../../;ll"
alias kkkkkkkkkkkkk=" cd ../../../../../../../../../../../../../;ll"
alias kkkkkkkkkkkkkk=" cd ../../../../../../../../../../../../../../;ll"

# Custom aliases

alias apush="~/syscfg/script/sys/apush.sh"
alias c="~/syscfg/script/sys/utp.sh"
alias cls="echo -ne '\x1bc'" # clear the screen buffer (don't just hide it like CTRL-L)
alias fiv='~/syscfg/script/sys/fiv.sh'
alias aex='~/syscfg/script/sys/aex.sh'
alias x='~/syscfg/script/sys/terms/wb.sh D'
alias up='~/syscfg/script/sys/up'
alias fop='~/prog/fop/dist/build/fop/fop'
alias cascade='~/syscfg/script/sys/cascade.sh'
alias initkeys="~/syscfg/script/sys/initkeys.sh"
alias INITKEYS="~/syscfg/script/sys/initkeys.sh" # for cases when capslock can't be turned off
alias switch_audio="~/syscfg/alsa/switch_audio.sh"
alias private=' HISTFILE=/tmp/zsh-history'
alias unprivate=' HISTFILE=~/.zsh-untracked/history'

alias d='univ_open'
alias df=' df -h'
alias dmesg='dmesg --color=always | less'
alias du='du -h'
alias dus='du -ah --max-depth 1 | sort -h'

alias enc='gpg2 -e -r linusarver'
alias dec='~/syscfg/script/sys/decrypt.sh outfile'
alias decless='~/syscfg/script/sys/decrypt.sh viewfile'

# sanitize directory/file permissions
alias sanitize_files='find -type d -exec chmod 755 {} \; ; find -type f -exec chmod 644 {} \;'

# easy find command (see find_quick() function)
alias fa='find_quick a '
alias fA='find_quick aa '
alias ff='find_quick f '
alias fF='find_quick ff ' # can't use 'find_quick F ' because the F is globally aliased itself (down below)
alias fd='find_quick d '
alias fD='find_quick dd '
alias ft='find_quick t '
alias fT='find_quick tt '

alias  ll=' dir_info 0'
alias   l=' dir_info 1'
alias llj=' dir_info 2'
alias  lj=' dir_info 3' # sort by size, and reverse it (bigger files @ bottom)
alias llk=' dir_info 4' # sort by time, and reverse it (latest files @ bottom)
alias  lk=' dir_info 5'
alias lli=' dir_info 6' # sort by extension
alias  li=' dir_info 7'

alias q=' exit'

alias mp='mpv -playlist'
alias make_plist="~/syscfg/script/audio/make_plist.sh"

alias rsa="rand_open norecurse 'mpv -vo null -playlist' '' $ft_snd"
alias rsaa="rand_open recurse 'mpv -vo null -playlist' '' $ft_snd"
alias rsp="rand_open norecurse 'feh --auto-zoom --fullscreen --draw-filename --filelist' '' $ft_pic"
alias rspp="rand_open recurse 'feh --auto-zoom --fullscreen --draw-filename --filelist' '' $ft_pic"
alias rsm="rand_open norecurse mpv m3u $ft_mov"
alias rsmm="rand_open recurse mpv\ --loop-file\ inf\ \ --playlist m3u $ft_mov"

alias g='git'
alias gbr='git branch'
alias gcm='git commit'
alias gcma='git commit --amend'
# The "f" here is for "fixing up" the last commit (i.e., equivalent of "git
# commit; git rebase HEAD~2" then marking the tip commit as "f" for fixup.
alias gcmf='git commit --amend --no-edit'
alias gcmm='git commit -m'
alias gcmu='git commit -m update'
alias gco='git checkout'
alias gft='git fetch'
alias glg='git log'
alias glgp='git log --patch'
alias gpl='git pull'
alias gplr='git pull --rebase'
alias gm='git merge'
alias gmf='git merge --ff-only'
alias ga='git add'
alias gap='git add --patch'
alias gau='git add --update'
alias grb='git rebase'
alias grbc='git rebase --continue'
alias grbi='git rebase -i'
alias grp='git reset --patch'
alias gps='git push'
alias gp='gsy -p '
alias gre='git rerere'
alias gtag='~/syscfg/script/sys/gtag.sh'

alias tig='tig -n1000'

alias ocm="cd ~/org; gcm -am \"$HOST\"; gpl && gps"

alias e='emacs_open'
alias ee='gvim -p'
alias v='vim -p'

alias -g galias_find_src='$(find -maxdepth 5 -type f -iregex ".*\.\(c\|cpp\|h\|hpp\|factor\|hs\|lhs\|pl\|py\|rb\|sh\)$" | sort)'
alias es=' e galias_find_src'
alias ees=' ee galias_find_src'

alias fl='find_long_lines '

alias skype='find ~/.Skype/Logs/ -type f -exec rm {} \; && skype'

# these sane hostnames depend on the /etc/hosts file!
alias sk0='ssh l@k0'
alias sk1='ssh l@k1'
alias sl0='ssh l@l0'
alias sm0='ssh l@m0'

# GLOBAL, position-independent aliases for detaching a process from the shell
# (useful for starting GUI apps as standalones, without arguments).
alias -g D='& disown'
alias -g Q='& disown; exit'

# for quiet processes
alias -g Z='> /dev/null 2>&1'
alias -g L='| less'

# scp alternative, with a progress bar
alias rsy='rsync -ahP --no-whole-file --inplace'
#'rscp' means 'cp' with 'rsync'. The '-a' (archive) option ensures that all
#file properties (date, permissions, owner, etc.) are copied, too, recursively.
#'-P' enables the progress bar. The next two options are a matter of taste but
#are useful if you copy files to a USB stick: '--inplace' writes updated data
#directly into the target file if it exists (instead of creating a temp copy,
#deleting the target, and renaming the temp into the target). '--no-whole-file'
#forces rsync to always use the incremental rsync algorithm.
alias cp2='rsync -ahP'

# play safe! warn before overwrites, display verbose output, and also enable
# recursive option by default (useful when using only directory names)
alias cp='cp -ivr'
alias mv='mv -iv'
alias rm='rm -Iv'

# clean up botched permissions
alias clean_dirs='find -type d -exec chmod 755 {} \;'
alias clean_files='find -type f -exec chmod 644 {} \;'

# sync the following with visudo to disable passwords!
#
# Template (in visudo):
#
# user  hostname=NOPASSWD:  path-to-command
#
# E.g.:
#
# l k2=NOPASSWD: /usr/sbin/iftop

# turn off power or reboot gracefully
alias of=' poweroff & disown; exit' # power off
alias ofo=' reboot & disown; exit' # reboot

alias n='nix-env'
alias nl='nix-env --list-generations'
alias np='ls -l --color=always ~/.nix-profile'
alias nq='nix-env -qaP'
alias nls='sudo nix-env -p /nix/var/nix/profiles/system --list-generations'

alias cal='cal -y'

nixos_hosts=(k0 k3 w0)

# memory management
alias reset_cache='free && sync && echo 3 >/proc/sys/vm/drop_caches && free'
alias reset_swap='free && swapoff -a && swapon -a && free'

# CUSTOM SCRIPTS
#
# encoding/decoding
alias rgmp3='~/syscfg/script/audio/replaygain/mp3/tmwrg.sh'
alias rgflac='~/syscfg/script/audio/replaygain/flac/tfwrg.sh'

# mount/unmount USB drives
alias usb='~/syscfg/script/sys/usbmnt -d'
alias usbon='~/syscfg/script/sys/usbmnt'
alias usbonn='~/syscfg/script/sys/usbmnt -a'
alias usbof='~/syscfg/script/sys/usbmnt -u'
alias usboff='~/syscfg/script/sys/usbmnt -U'

case $HOST in
	k0)
		alias iftop='sudo iftop -B -i eno1'
		alias discon='sudo mount /dev/disk/by-id/ata-PIONEER_DVD-RW_DVR-215D'
		alias discof='sudo umount /dev/disk/by-id/ata-PIONEER_DVD-RW_DVR-215D'
		alias disc2on='sudo mount /dev/disk/by-id/ata-LITE-ON_DVDRW_SHW-160P6S'
		alias disc2of='sudo umount /dev/disk/by-id/ata-LITE-ON_DVDRW_SHW-160P6S'
	;;
	k1)
		alias iftope='sudo iftop -B -i eth0'
		alias iftopw='sudo iftop -B -i wlan0'
		alias discon='sudo mount /dev/sr0'
		alias discof='sudo umount /dev/sr0'
		alias nwu='sudo netcfg -c aether-wireless-home'
		alias nwd='sudo netcfg -d aether-wireless-home'
	;;
	w0)
		alias yk='gpg-connect-agent updatestartuptty /bye'
	;;
	*)
		alias discon='sudo mount /dev/sr0'
		alias discof='sudo umount /dev/sr0'
	;;
esac

# Get rid of odd ^[[2004h characters from Emacs' `M-x shell'. The problem has to
# do with ZSH trying to set bracketed paste mode which came out in ZSH 5.1.1,
# even though the "dumb" terminal that `M-x shell' sets is unable to handle it.
# See https://github.com/syl20bnr/spacemacs/issues/3035.
if [[ $TERM == "dumb" ]]; then
    unset zle_bracketed_paste
fi
