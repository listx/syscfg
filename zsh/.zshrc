# ZSH configuration.
#
# For reference, ZSH configuration load order is:
#
# 1. /etc/zshenv.
# 2.   ~/.zshenv
# 3. (if login shell) /etc/zprofile
# 4. (if login shell)   ~/.zprofile
# 5. (if interactive) /etc/zshrc
# 6. (if interactive)   ~/.zshrc
# 7. (if login shell) /etc/zlogin
# 8. (if login shell)   ~/.zlogin
#
# /etc/zlogout and ~/.zlogout are also read, but only if the shell is a login
# shell and also only after the shell exits. As such they are not that useful
# in practice.

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
ft_snd=(aac au flac m4a mid midi mka mp3 mpc ogg opus ra wav axa oga spx xspf)
for t in $ft_ark; do LS_COLORS+="*.$t=32:"; done
for t in $ft_pic; do LS_COLORS+="*.$t=35:"; done
for t in $ft_mov; do LS_COLORS+="*.$t=1;35:"; done
for t in $ft_snd; do LS_COLORS+="*.$t=34:"; done
export LS_COLORS

# colors for the 'less' pager
export LESS_TERMCAP_md=$'\x1b[1;34m'      # begin bold
export LESS_TERMCAP_mb=$'\x1b[1;31m'      # begin blinking
export LESS_TERMCAP_me=$'\x1b[0m'         # end bold/blinking

export LESS_TERMCAP_us=$'\x1b[1;35m'      # begin underline
export LESS_TERMCAP_ue=$'\x1b[0m'         # end underline

export LESS_TERMCAP_so=$'\x1b[1;30;42m'   # begin standout-mode - (interactive search highlight)
export LESS_TERMCAP_se=$'\x1b[0m'         # end standout-mode

# Set PAGER. This is not set on Mac, which results in /usr/bin/less (hardcoded
# path) being executed. This hardcoded behavior appears to be by design, as it
# is stated as such in the manpage.
export PAGER=less

# Default text editor--set it to emacsclient.
export VISUAL=~/syscfg/script/emacsclient-tty.sh
export EDITOR=~/syscfg/script/emacsclient-tty.sh

# default ledger file (to avoid using -f option every time)
export LEDGER_FILE=~/lo/finance/index.ledger

# make GCC print errors in color
export GCC_COLORS=1

# path to local nixpkgs git repo
export NIXPKGS=~/prog/foreign/nixpkgs

# Add Golang binaries to the PATH.
export PATH=~/go/bin:$PATH

# Add Rust binaries (installed with "cargo install" to the PATH.
export PATH=~/.cargo/bin:$PATH

# Stack (https://www.haskellstack.org/) needs this.
export PATH=~/.local/bin:$PATH

# Load executables for "cabal v2-install".
export PATH=~/.cabal/bin:$PATH

# Add system scripts to the PATH.
export PATH=~/syscfg/script:$PATH

# stop zsh from eating space before pipe symbol
export ZLE_REMOVE_SUFFIX_CHARS=""

# 10ms for ESC key vs ALT+... disambiguation.
export KEYTIMEOUT=1

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

# Make the prompt re-evaluate itself every second. Among other things, this
# makes the clock tick every second. See https://askubuntu.com/a/360172.
setopt PROMPT_SUBST
TMOUT=1

__l_get_mem()
{
    ps -o rss= -p $$
}

# Don't redraw the prompt if we are in the middle of a completion widget. See
# https://stackoverflow.com/a/30456173/437583.
__l_prompt_tick ()
{
    # Don't redraw the prompt if we've typed *anything* into the command line.
    # This way we can at least save an approximate timestamp of when we last
    # started typing.
    if (( $#BUFFER > 0 )); then
        return
    fi

    case "$WIDGET" in
        # Don't call reset-prompt if we are using fzf widgets. This checks 4
        # known cases:
        #   fzf-cd-widget (ALT-D)
        #   fzf-history-widget (CTRL-R)
        #   fzf-file-widget (CTRL-T)
        #   fzf-completion (TAB-completion)
        fzf-*) return ;;
        # Multi-line input with "\" character at the end.
        accept-line) (( $#BUFFER > 0 )) && return ;;
    esac

    # There seems to be a memory leak in zsh 5.8 (x86_64-apple-darwin17.7.0),
    # where constantly resetting the prompt eats up memory. We've observed the
    # shell ballooning to over 1GiB in memory usage after several days.
    #
    # As a temporary workaround, do not reset the prompt if our memory usage is
    # too high (>50MB).
    (( $(__l_get_mem) > 50000 )) && return

    zle reset-prompt
}

TRAPALRM() {
    __l_prompt_tick
}

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
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}" # use the LS_COLORS variable to colorize the completion list
LISTMAX=9999 # don't ask 'do you wish to see all N possibilities' unless N > 9999

# make '*' expand to catch dotfiles as well
setopt GLOB_DOTS
# Among other things, allow use of things like "ls ^(foo)*" to match all files
# that do not start with "foo".
setopt extendedglob

# Key bindings.

bindkey -v # use vim bindings!
bindkey     '^[[3~'         delete-char         # DELETE key
bindkey     '^[[7~'         beginning-of-line   # HOME key
bindkey     '^[[8~'         end-of-line         # END key
bindkey -M menuselect '' .accept-line # just execute the command when selecting from a menu and pressing <enter>
#bindkey     '\t' menu-expand-or-complete   # press <tab> just ONCE to bring up menu *AND* select the first item

# Reclaim "^s" (C-s) from terminal to Zsh. Traditionally, terminals pause
# output with C-s and resume output with C-q, but these hotkeys are rarely
# used.
stty stop undef
stty start undef

# Quick-access functions to invoke standardized tmux sessions. The
# ${funcstack[1]} in Zsh holds the calling function's name.
admin() {
  run_with_theme Red.dhall tmux new-session -A -s "${funcstack[1]}"
}

# make 'ds' prompt the user to select a dir from the dir stack, instead of just listing the dir stick
# with a plain 'dirs -v'
# NOTE: 'cd +$n' makes it go to the number listed, whereas 'cd -$n' reverses it... don't know why
# you'd ever want to use 'cd -$n'
alias ds=' dirs -v ; echo -n "\nSelect directory: " ; read n ; cd +$n && ll'

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

alias c="color_cycle next"
alias C="color_cycle prev"
alias cls="echo -ne '\x1bc'" # clear the screen buffer (don't just hide it like CTRL-L)
alias fiv='~/syscfg/script/fiv.sh'
alias aex='~/syscfg/script/aex.sh'
alias cascade='~/syscfg/script/cascade.sh'
alias private=' HISTFILE=/tmp/zsh-history'
alias unprivate=' HISTFILE=~/.zsh-untracked/history'

alias d='univ_open'
alias df=' df -h'
alias dmesg='dmesg --color=always L'
alias du='du -h'

alias enc='gpg2 -e -r linusarver'
alias dec='~/syscfg/script/decrypt.sh outfile'

alias rot13='tr 'A-Za-z' 'N-ZA-Mn-za-m''

alias  ll=' dir_info brief'
alias   l=' dir_info verbose-by-name'
alias  lj=' dir_info verbose-by-size'
alias  lk=' dir_info verbose-by-date'

alias q=' exit'

alias ma='mpv --vo=null'
alias mp='mpv -playlist'

alias rsa="rand_open norecurse 'mpv -vo null -playlist' '' $ft_snd"
alias rsaa="rand_open recurse 'mpv -vo null -playlist' '' $ft_snd"
alias rsp="rand_open norecurse 'feh --auto-zoom --fullscreen --draw-filename --filelist' '' $ft_pic"
alias rspp="rand_open recurse 'feh --auto-zoom --fullscreen --draw-filename --filelist' '' $ft_pic"
alias rsm="rand_open norecurse mpv m3u $ft_mov"
alias rsmm="rand_open recurse mpv\ --loop-file=inf\ \ --playlist m3u $ft_mov"

alias g='git'
alias gbr='git branch'
alias gc='git commit'
alias gca='git commit --amend'
alias gcan='git commit --amend --no-edit'
alias gcm='git commit -m'
alias gcu='git commit -m update'
alias gco='git checkout'
alias gcs='git_copy_sha'
alias gft='git fetch'
alias gflip='git_rebase_range HEAD~2 HEAD'
alias gl='git log'
alias glp='git log --patch'
alias gpl='git pull --ff-only'
alias gplm='git pull'
alias gplr='git pull --rebase'
alias gm='git merge'
alias gmf='git merge --ff-only'
alias ga='git add --patch'
alias gau='git add --update'
alias gu='gau && gcu && gp' # Useful for saving work immediately in note-taking repos.
alias gr='git rebase'
alias grc='git rebase --continue'
alias gri='git rebase -i'
alias gsp='git reset --patch'
alias gee='git rerere'
alias gp='git push'
alias grv='git remote -v'
alias gtag='~/syscfg/script/gtag.sh'

alias e='emacs_open'
alias ekb='emacs_kill_buffer'
alias elb='emacs_list_buffers'
alias v='nvim -p'

alias fl='find_long_lines '
alias ft='f t'
alias ff='f f'

# Make ssh pretend we are using xterm, because some machines do not recognize
# our usual terminal emulator (rxvt-unicode-256coler).
alias ssh='TERM=xterm-256color ssh'
# These hostnames depend on either /etc/hosts or ~/.ssh/config.
alias sk0='ssh l@k0'
alias sk1='ssh l@k1'
alias sm0='ssh l@m0'
alias sw0='ssh ingra@w0'
alias k0='ssh l@k0     -t -- tmux new-session -A -s k0'
alias k1='ssh l@k1     -t -- tmux new-session -A -s k1'
alias m0='ssh l@m0     -t -- tmux new-session -A -s m0'
alias w0='ssh ingra@w0 -t -- tmux new-session -A -s w0'

# GLOBAL, position-independent aliases for detaching a process from the shell
# (useful for starting GUI apps as standalones, without arguments).
alias -g D='& disown'
alias -g L='| less'

# scp alternative, with a progress bar
alias rsy='rsync -ahP --no-whole-file --inplace'
# The '-a' (archive) option ensures that all file properties (date,
# permissions, owner, etc.) are copied, too, recursively.  '-P' enables the
# progress bar. The next two options are a matter of taste but are useful if
# you copy files to a USB stick: '--inplace' writes updated data directly into
# the target file if it exists (instead of creating a temp copy, deleting the
# target, and renaming the temp into the target). '--no-whole-file' forces
# rsync to always use the incremental rsync algorithm.

# Warn before overwrites, display verbose output, and also enable recursive
# option by default (useful when using only directory names).
alias cp='cp -ivr'
alias mv='mv -iv'
alias rm='rm -Iv'

# Clean up botched permissions.
alias clean_dirs='find -type d -exec chmod 755 {} \;'
alias clean_files='find -type f -exec chmod 644 {} \;'
alias clean_tree='clean_dirs; clean_files'

# Turn off power or reboot gracefully.
alias of=' poweroff & disown; exit'
alias ofo=' reboot & disown; exit'

alias nl='nix-env --list-generations'
alias nls='sudo nix-env -p /nix/var/nix/profiles/system --list-generations'
alias nq='nix-env -qaP'

alias agi="sudo apt-get install"
alias agr="sudo apt-get remove"
alias acs="apt_cache_search"

alias cal='cal -y'

# Use python3 by default (python2 is EOL as of 2020)
alias python=python3

# Manual memory management.
alias reset_cache='free && sync && echo 3 >/proc/sys/vm/drop_caches && free'
alias reset_swap='free && swapoff -a && swapon -a && free'

# CUSTOM SCRIPTS
#
# encoding/decoding
alias rgmp3='~/syscfg/script/audio/replaygain/mp3/tmwrg.sh'
alias rgflac='~/syscfg/script/audio/replaygain/flac/tfwrg.sh'

# Add commonly-used paths as aliases.
. ~/.zsh/path-aliases

# Get rid of odd ^[[2004h characters from Emacs' `M-x shell'. The problem has to
# do with ZSH trying to set bracketed paste mode which came out in ZSH 5.1.1,
# even though the "dumb" terminal that `M-x shell' sets is unable to handle it.
# See https://github.com/syl20bnr/spacemacs/issues/3035.
if [[ $TERM == "dumb" ]]; then
    unset zle_bracketed_paste
fi

# ZSH Plugins with ZPlug. To install ZPlug, see https://github.com/zplug/zplug.
if [[ $HOST =~ macbook ]]; then
    source "$HOME"/.nix-profile/init.zsh
else
    source ~/.zplug/init.zsh
fi

zplug "MichaelAquilina/zsh-you-should-use"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# This has to happen *before* loading custom completions.
zplug load

# Load in completions for Google Cloud SDK. For some reason, this has to be
# done *after* the call to 'zplug load'.
google_cloud_sdk_path="${L_GOOGLE_CLOUD_SDK_PATH:-}"
if [[ -n "${google_cloud_sdk_path}" ]]; then
    f="${google_cloud_sdk_path}/completion.zsh.inc"
    source "${f}" || echo "could not source ${f}"
fi

# For some reason loading it from ~/.zprofile-enif leads to a cryptic
#
#   complete:13: command not found: compdef
#
# error. Loading completion code last, as in
# https://github.com/robbyrussell/oh-my-zsh/issues/6163#issuecomment-315836297,
# makes it go away.
if [ "${commands[kubectl]}" ]; then
    source <(kubectl completion zsh)
    # Pass through the default kubectl completions to kl (zsh/func/kl).
    compdef kl=kubectl
fi

# Load fzf bindings (upstream) to ZSH.
if [[ -n "${commands[fzf-share]}" ]]; then
	source "$(fzf-share)/completion.zsh"
	source "$(fzf-share)/key-bindings.zsh"

    # Use ALT-D binding instead of the default ALT-C.
    bindkey '\ed' fzf-cd-widget
    export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND='rg --files --hidden --glob "!.git" --null | xargs -0 dirname | sort -u'
fi
