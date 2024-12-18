#!/usr/bin/env zsh

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

  construct_prompt
  zle reset-prompt
}

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

__l_command_not_found () {
  # $global_exit_status is set by the prompt. If we simply use "$?" as the variable
  # to check, unfortunately it does not get cleared when we (1) invoke an
  # unknown command, then (2) delete the contents of BUFFER loaded by `hist -fs
  # f -1'.
  if (( global_exit_status == 127 )) ; then
    hist -fs f -1 || echo "\`hist' is not installed"
  fi
}

__l_maybe_load_completions_and_autocorrect()
{
  # Remove leading whitespace from BUFFER, to catch cases where we enter some
  # spaces or tabs before actually typing the command name. See
  # https://stackoverflow.com/a/3352015.

  # The BUFFER may be something that we type from scratch (where it may have
  # some leading whitespace), or it may be something we load in from C-r
  # which already has many words in it. In both cases we want to (1) remove any
  # leading whitespace, and (2) isolate the first word found. This
  # whitespace-free first word is the name of the command we want to complete
  # (and check completions for).
  local leading_whitespace
  local cmd_all_words
  local cmd_first_word
  local cmd_remaining_words
  leading_whitespace="${BUFFER%%[![:space:]]*}"
  # Remove leading whitespace.
  cmd_all_words="${BUFFER#${leading_whitespace}}"
  # Get the first word without the leading whitespace (if any).
  cmd_first_word="${cmd_all_words%%[[:space:]]*}"
  # Get remaining words.
  cmd_remaining_words="${cmd_all_words#${leading_whitespace}${cmd_first_word}}"
  __l_run_fzf=0
  case "${cmd_first_word}" in
  d)
    if [[ "${BUFFER}" == "d " ]]; then
      __l_run_fzf=1
    fi
    ;;
  kl|kubectl)
    # The first word could be either "kl " or "kubectl". For "kl", expand it out
    # to "kubectl" because it's more explicit and better for copy-pasting to
    # others who may not use the same abbreviations as us.  Preserve any leading
    # whitespace and trailing words.
    if [[ "${cmd_first_word}" == "kl" ]]; then
      BUFFER="${leading_whitespace=}kubectl${cmd_remaining_words=}"
      # Move to the end of the line because our cursor is at the end of "l" in
      # "kl" and we just put in the longer "kubectl".
      zle end-of-line
    fi

    # For the "kubectl" case, we'll see this case if we are doing
    # C-r and going back through history where we typed in "kl" but it was
    # replaced (by us) with "kubectl" when we pressed TAB or space after "kl"
    # earlier, but in another shell process (history is saved across
    # processes). There is nothing extra to do because "kubectl" is already what
    # we want.

    if (( "${__l_already_loaded_kl_comps:-0}" )); then
      return
    fi

    source <(command kubectl completion zsh)
    __l_already_loaded_kl_comps=1
    ;;
  "watch kl")
    BUFFER="${leading_whitespace}watch kubectl"
    ;;
  *)
    ;;
  esac
}

__l_lazy_load_completions()
{
  __l_maybe_load_completions_and_autocorrect

  if ((__l_run_fzf)); then
    fzf-file-and-enter-widget
    return
  fi
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

# Check if the current shell session is an SSH session (a remote session).
__l_is_ssh_connection()
{
  if [[ -n "${SSH_CLIENT:-}" ]] || [[ -n "${SSH_TTY:-}" ]]; then
    return
  fi

  case $(ps -o comm= -p "$PPID") in
    sshd|*/sshd) return;;
  esac

  return 1
}

# Grab hostname. If we're using our own home-grown convention of using
# ~/.hostname-short as a shorter hostname alias for scripting, use that instead
# if it is available.
__l_get_hostname()
{
  local _hostname

  _hostname=$(hostname)
  if [[ -f ~/.hostname-short ]]; then
    _hostname=$(cat ~/.hostname-short)
  fi

  echo "${_hostname}"
}

__l_get_tmux_sessions()
{
  local _hostname

  _hostname="${1}"

  tmux list-sessions \
    | cut -d: -f1 \
    | grep "^${_hostname}-[0-9]\+\$" \
    | sort
}

__l_get_tmux_sessions_attached()
{
  local _hostname

  _hostname="${1}"

  tmux list-clients \
    | cut -d' ' -f2 \
    | grep "^${_hostname}-[0-9]\+\$" \
    | sort
}

# Return the smallest free available session ID for tmux, using the template
# <HOSTNAME>-<NUMBER>. This is the "parking lot" problem described at
# https://funloop.org/post/2016-09-24-parking-lot-problem-revisited.html.
#
# One slight tweak is that instead of always creating a "next highest number"
# desired_id, we prioritize the sessions that are not connected by a terminal.
# This way we never "abandon" a tmux session by creating a new session. We
# basically never want to have unattached sessions.
__l_find_free_tmux_session_id()
{
  local desired_id=0
  local session_ids
  local _hostname

  _hostname="${1}"

  # Collect all existing attached session names to try to pick the first free
  # (unattached) session id.
  #
  # (f) causes the output to be split on newlines.
  session_ids=(${(f)"$(__l_get_tmux_sessions_attached ${_hostname})"})

  # Note that in Zsh, arrays indices start from 1, not 0.
  if [[ -n "${session_ids[1]:-}" ]]; then
    for session_id in "${session_ids[@]}"; do
      if (( desired_id < ${session_id##*-} )); then
        break
      else
        ((desired_id++))
      fi
    done
  fi

  echo "${desired_id}"
}

__l_new_tmux_session_command()
{
  local _hostname
  local desired_id

  _hostname="${1}"
  desired_id="${2}"

  echo "tmux new-session -A -s ${_hostname}-${desired_id}"
}

# Invoke tmux, but find the smallest free number among the existing session
# names, and use that. This avoids having always-increasing session numbers,
# which can get out of hand if we're invoking tmux many times on a machine
# that's on for weeks or months.
#
# We prefer to use default session names where the format is
# <hostname>-<session_id>, and <session_id> is the smallest number possible.
#
# See https://funloop.org/post/2016-09-24-parking-lot-problem-revisited.html for
# a discussion about optimal ways to find the smallest free number. We just use
# a brute-force approach here because (1) we're lazy and (2) this is shell code.
__l_tmux_command()
{
  local desired_id=0
  local session_ids
  local _hostname

  _hostname="$(__l_get_hostname)"

  # If connecting over SSH, don't try to create a new session, because chances
  # are that we want to just reattach to an existing session. Users can create a
  # separate, new tmux session manually if they need one.
  if __l_is_ssh_connection; then

    # Collect all existing session names to try to pick the first free session
    # id.
    #
    # (f) causes the output to be split on newlines.
    session_ids=(${(f)"$(__l_get_tmux_sessions ${_hostname})"})

    if [[ -n "${session_ids[1]:-}" ]]; then
      # Get last element of array.
      desired_id=${session_ids[-1]##*-}
    fi

    __l_new_tmux_session_command "${_hostname}" "${desired_id}"
    return
  fi

  desired_id="$(__l_find_free_tmux_session_id "${_hostname}")"
  __l_new_tmux_session_command "${_hostname}" "${desired_id}"
}

# Use this to avoid putting in duplicate entries into the $PATH. This can happen
# if you log in with zsh as your login shell (resulting in reading ~/.zshrc),
# and then invoke tmux (which itself invokes zsh again, as a login shell). This
# scenario will result in ~/.zshrc being read 2x --- so if you have plain
# "export PATH=..." statements, it will result in a duplicate entry.
#
# To avoid duplicate entries, we only prepend to the path if we have a non-empty
# string and it isn't already in the PATH.
__l_prepend_path()
{
  local dir="${1:-}"
  local tmp_path

  [[ -z "${dir}" ]] && return

  # If dir is already in the PATH, move it to the front.
  if [[ "${PATH}" == *"${dir}"* ]]; then
    # Catch edge case where we want to promote the very last dir.
    tmp_path="${PATH}:"
    tmp_path="${tmp_path//${dir}:/}"
    # Remove trailing ":".
    export PATH="${dir}:${tmp_path%:}"
    return
  fi

  export PATH=${dir}:$PATH
}

# Start a new tmux session in the background, then switch to it. Note that this
# is different than using `tmux attach-session ...` because we do *NOT* start a
# nested tmux session.
t()
{
  local desired_id=0
  local _hostname
  local session_id

  _hostname="$(__l_get_hostname)"
  desired_id="$(__l_find_free_tmux_session_id "${_hostname}")"
  session_id="${_hostname}-${desired_id}"

  tmux new-session -d -s "${session_id}"
  tmux switch-client -t "${session_id}"
}

# Yank to the system clipboard.
__l_vi_yank_system_clipboard() {
  # Execute the normal copying widget to copy to Zsh's own register.
  zle vi-yank
  # Also copy to the system clipboard.
  echo "$CUTBUFFER" | >/dev/null ~/syscfg/script/copy-clipboard.sh
}
