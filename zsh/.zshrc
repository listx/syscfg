# ZSH configuration.
#
# To profile Zsh startup time, uncomment this and also call `zprof' at the end
# of this file.
#zmodload zsh/zprof
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

# special file types (compare with 'dircolors -b')
LS_COLORS='rs=0:di=01;33:mh=00:pi=01;32:so=01;34:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:st=37;44:'
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

fpath=(~/.zsh/func $fpath) # add ~/.zsh/func to $fpath
autoload -U ~/.zsh/func/*(:t) # load all functions in ~/.zsh/func

autoload -U promptinit
promptinit

autoload -U zmv

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
	# Don't redraw the prompt if we are inside an Emacs vterm session, because
	# redrawing it forces the cursor to move to the bottom of the screen after
	# each redraw even if we're in Evil's "normal" mode, rendering normal mode
	# useless.
	if [[ -n "${EMACS_VTERM_PATH:-}" ]]; then
		return
	fi

    # Don't redraw the prompt if we've typed *anything* into the command line.
    # This way we can at least save an approximate timestamp of when we last
    # started typing.
    if (( ${#BUFFER} > 0 )); then
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
HISTSIZE=50000
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

# Find stuff.
alias fl='find_long_lines '
alias ff='f files'
alias ffa='f all_files'
alias fp='f procs'
alias ft='f text'
# Search all text files, including normally hidden/ignored ones.
alias fta='f all_text'
# Search even inside binary files.
alias ftab='f all_text_binary'

# Make ssh pretend we are using xterm, because some machines do not recognize
# our usual terminal emulator (rxvt-unicode-256coler).
alias ssh='TERM=xterm-256color ssh'
# These hostnames depend on either /etc/hosts or ~/.ssh/config.
alias sk0='s k0 '
alias sk1='s k1 '
alias sm0='s m0 '
alias sw0='s w0 '

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
alias reset_cache='free && sync && sudo echo 3 >/proc/sys/vm/drop_caches && free'
alias reset_swap='free && sudo swapoff -a && sudo swapon -a && free'

# CUSTOM SCRIPTS
#
# encoding/decoding
alias rgmp3='~/syscfg/script/audio/replaygain/mp3/tmwrg.sh'
alias rgflac='~/syscfg/script/audio/replaygain/flac/tfwrg.sh'

# Add commonly-used paths into the named directory hash table.
. ~/.zsh/path-aliases
# Wrap around default accept-line zle widget in order to do special handling of
# `d.<aliased_directory>', where `<aliased_directory' comes from
# ~/zsh/path-aliases. See https://stackoverflow.com/a/28101424.
__l_accept_line () {
  # Check for a command that starts with `d.'.
  if [[ "$BUFFER" == "d."* ]]; then
    # Only convert to a named directory buffer if we get a match of the named
    # directory in the hash table.
    local dir_alias
    local dir_expanded_alias
    dir_alias="${BUFFER#d.}"
    dir_expanded_alias=$(hash -dm "${dir_alias}")
    dir_expanded_alias="${dir_expanded_alias#*=}"
    if [[ -e "${dir_expanded_alias}" ]]; then
		BUFFER="d ~${dir_alias}"
    else
		echo -e >&2 "\nunrecognized directory alias ${(q)dir_alias}"
    fi
  fi
  # Call original `accept-line' widget by prepending the leading period.
  zle .accept-line
}
zle -N accept-line __l_accept_line

# Get rid of odd ^[[2004h characters from Emacs' `M-x shell'. The problem has to
# do with ZSH trying to set bracketed paste mode which came out in ZSH 5.1.1,
# even though the "dumb" terminal that `M-x shell' sets is unable to handle it.
# See https://github.com/syl20bnr/spacemacs/issues/3035.
if [[ $TERM == "dumb" ]]; then
    unset zle_bracketed_paste
fi

# Use Vim bindings! (Note: this command must come first before the other bindkey
# invocations).
bindkey -v

# Make "kj" in viins mode enter vi-cmd-mode (same as pressing ESC).
bindkey -M viins "kj" vi-cmd-mode
# 0.1s for ESC key vs ALT+... disambiguation. This is required for the "kj"
# binding above to work.
export KEYTIMEOUT=10

# Make Backspace delete past the point of the last point of viins insertion.
bindkey "^?" backward-delete-char

# Map DELETE key.
bindkey '^[[3~' delete-char
# Map HOME key. Natively in NixOS, we get the OH key sequence. If we don't
# bind this, this sequence gets interpreted as "ESC, O, H", where the O is the
# "open line above" keybinding under the Vim bindings --- so it is useless
# anyway. Over SSH, we get the ^[[1~ sequence.
bindkey 'OH'  beginning-of-line
bindkey '^[[1~' beginning-of-line
# Map END key. Natively in NixOS, we get the OF key, and over SSH we get
# ^[[4~.
bindkey "OF" end-of-line
bindkey '^[[4~' end-of-line

# Some directory history navigation. 'h' goes backward in history, and 'l' goes
# "forward". Essentially this amounts to simulating a doubly-linked-list
# traversal, forwards and backwards.
#
# We prepend these commands with a space to block them from being saved into
# the shell history (because of HIST_IGNORE_SPACE).
bindkey -s '^h' " dirs_navigate prev\n"
bindkey -s '^l' ' dirs_navigate next\n'
# Move up a directory. This is slightly faster than typing 'k' then 'Enter'. We
# can't bind C-j, because it is interpreted by Zsh to be the same thing as a
# newline. So binding it is the same thing as rebinding the newline character.
bindkey -s '^K' ' k\n'

zmodload zsh/complist # for the 'menuselect' keymap
# Just execute the command when selecting from a menu and pressing <enter>.
# Regarding the `.' in front of 'accept-line', the manpage for zshcompsys(1) has
# this to say:
#
#   Should you need to use the original completion commands, you can still bind
#   keys to the old widgets by putting a `.' in front of the widget name, e.g.
#   `.expand-or-complete'.
bindkey -M menuselect '' .accept-line

# Allow parameter expansion, command substitution and arithmetic expansion in
# prompts.
setopt PROMPT_SUBST

# This will set the default prompt to the kody theme
prompt kody

# Make C-e go to the end of the line. This also makes it accept the
# autosuggested completion, if there is any.
bindkey '^e' end-of-line
# For completeness, map C-a to go to the beginning of the line. This is like the
# default emacs bindings.
bindkey '^a' beginning-of-line
bindkey '^x' autosuggest-execute

# Prevent command typos from cluttering up history.
# See https://stackoverflow.com/a/66060510/437583.
autoload -Uz add-zsh-hook
command-not-found () {
	# $exit_status is set by the prompt. If we simply use "$?" as the variable
	# to check, unfortunately it does not get cleared when we (1) invoke an
	# known command, then (2) delete the contents of BUFFER loaded by `hist -fs
	# f -1'.
	if (( exit_status == 127 )) ; then
		hist -fs f -1
	fi
}
add-zsh-hook precmd command-not-found

# Load in completions for Google Cloud SDK.
google_cloud_sdk_path="${L_GOOGLE_CLOUD_SDK_PATH:-}"
if [[ -n "${google_cloud_sdk_path}" ]]; then
    f="${google_cloud_sdk_path}/completion.zsh.inc"
    source "${f}" || echo "could not source ${f}"
fi

# Load fzf bindings (upstream) to ZSH.
if [[ -n "${commands[fzf-share]}" ]]; then
	# Try to use fzf-tmux to use tmux split panes (if we are inside tmux).
	export FZF_TMUX=1

	source "$(fzf-share)/completion.zsh"
	source "$(fzf-share)/key-bindings.zsh"

	# Use exact matches (except for space characters; i.e., you can type "world
	# hello" and it will match a "hello world" string).
	FZF_CTRL_R_OPTS=" --exact"
	# Sort entries. This essentially just groups together similarly-named
	# commands to be together (that is, search hits that share the same prefix
	# will be shown together, instead of being shown separately if they were not
	# executed one after the other chronologically).
	FZF_CTRL_R_OPTS+=" --sort"
	# `--select-1' automatically selects the item if there's only one so that you
	# don't have to press enter key. Likewise, `--exit-0' automatically exits
	# when the list is empty.
	FZF_CTRL_R_OPTS+=" --select-1 --exit-0"
	# Enable preview window. This makes it so that we can see the full command
	# if it is too long to be displayed in a single line. We hide this preview
	# window by default, and bind the `?' key to display it if we want to.
	FZF_CTRL_R_OPTS+=" --preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"
	export FZF_CTRL_R_OPTS

	export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'

	# C-t fuzzy searches a file argument to the current command we're trying to build up.
	export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

	# Enable preview window for files.
	FZF_CTRL_T_OPTS=" --preview '(cat {} || tree -C {}) 2> /dev/null | head -200'"
	FZF_CTRL_T_OPTS+=" --select-1 --exit-0"
	export FZF_CTRL_T_OPTS

	# Use ALT-D binding instead of the default ALT-C.
	bindkey '\ed' fzf-cd-widget
	export FZF_ALT_C_COMMAND='rg --files --hidden --glob "!.git" --null | xargs -0 dirname | sort -u'

	# Preview directory entries.
	FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
	FZF_ALT_C_OPTS+=" --select-1 --exit-0"
	export FZF_ALT_C_OPTS
fi

# Use zpm. Run "zpm clean" if modifying the .zshrc file to clear out the cache.
if [[ ! -f ~/.zpm/zpm.zsh ]]; then
  git clone --recursive https://github.com/zpm-zsh/zpm ~/.zpm
fi
source ~/.zpm/zpm.zsh

# Print warning if we don't use an existing alias for a command.
zpm load "MichaelAquilina/zsh-you-should-use",async

# Fish-shell-like automatically-suggested completions.
zpm load "zsh-users/zsh-autosuggestions",source:zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=2,bold"

# History editing. This brings in the `hist' command .
zpm load "marlonrichert/zsh-hist",async

# Instead of using "qoomon/zsh-lazyload", we simply examine the letters that are
# already inserted into the zle buffer. If it is "kl" and we press either the
# Space or Tab key, we invoke our custom widget which loads completions for
# kubectl, but only if it is both installed and if the completions have not been
# sourced already. This is an improvement over
# https://github.com/kubernetes/kubernetes/issues/59078#issuecomment-363384825
# because even the initially-attempted invocation will get completions.
bindkey -M viins " " __l_lazy_load_completions
bindkey -M viins "^I" __l_lazy_load_completions # ^I is the TAB key
__l_maybe_load_completions()
{
	# Remove leading whitespace from BUFFER, to catch cases where we enter some
	# spaces or tabs before actually typing the command name. See
	# https://stackoverflow.com/a/3352015.
	case "${BUFFER#"${BUFFER%%[![:space:]]*}"}" in
	kl)
		if ! [[ $commands[kubectl] ]]; then
			return
		fi

		# The __start_kubectl is defined only if we've already sourced the
		# completions.
		if (( $+functions[__start_kubectl] )); then
			return
		fi

		source <(command kubectl completion zsh)
		# Pass through the default kubectl completions to kl (zsh/func/kl).
		compdef kl=kubectl
		;;
	*)
		;;
	esac
}
__l_lazy_load_completions()
{
	__l_maybe_load_completions
	# Now invoke the vanilla zle widget that was supposed to have been called
	# from viins mode.
	case "${KEYS[-1]}" in
	" ") zle .self-insert
		;;
	"	") zle fzf-completion
		;;
	# We'd never reach this branch, unless we bound our keys wrong. Since we
	# have nothing better to do, just insert KEYS into the zle buffer.
	*) zle .self-insert
		;;
	esac
}
zle -N __l_maybe_load_completions
zle -N __l_lazy_load_completions

# Uncomment to profile.
#zprof

# Invoke tmux, but find the smallest free number among the existing session
# names, and use that. This avoids having always-increasing session numbers,
# which can get out of hand if we're invoking tmux many times on a machine
# that's on for weeks or months.
#
# See https://funloop.org/post/2016-09-24-parking-lot-problem-revisited.html for
# a discussion about optimal ways to find the smallest free number. We just use
# a brute-force approach here because (1) we're lazy and (2) this is shell code.
__l_tmux_command()
{
    local desired_id=0
    local session_ids
    # (f) causes the output to be split on newlines.
    session_ids=(${(f)"$(tmux list-sessions | cut -d: -f1 | grep '^[0-9]\+$' | sort)"})
    for session_id in "${session_ids[@]}"; do
        if (( desired_id < session_id )); then
            break
        else
            ((desired_id++))
        fi
    done
    echo "tmux new-session -A -s ${desired_id}"
}

# Replace current shell process with tmux, because it's much nicer to use tmux
# than a raw shell instance.
if command -v tmux &> /dev/null \
    && [[ -n "$PS1" ]] \
    && [[ ! "$TERM" =~ tmux ]] \
    && [[ -z "$TMUX" ]]; then
  exec $(__l_tmux_command)
fi
