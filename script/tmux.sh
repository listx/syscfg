#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

get_default_session_id()
{
  local desired_id=0
  local session_ids
  local _hostname="$1"

  # Prefer to use defaul session names where the format is
  # <hostname>-<session_id>, and <session_id> is the smallest number possible.
  readarray -t session_ids <<<"$(tmux list-sessions | cut -d: -f1 | grep "^${_hostname}-[0-9]\+\$" | sort)"
  if [[ -n "${session_ids[0]:-}" ]]; then
    for session_id in "${session_ids[@]}"; do
      echo >&2 $session_id
      if (( desired_id < ${session_id##*-} )); then
        break
      else
        ((desired_id++))
      fi
    done
  fi

  echo "${_hostname}-${desired_id}"
}

main()
{
  local host
  host="$(hostname)"
  local host_session

  # If there's a shorter name available, use it.
  if [[ -f ~/.hostname-short ]]; then
    # Only read the first 5 bytes. It better be short!
    host=$(head -c 5 ~/.hostname-short)
  fi

  # Replace non-tmux-session-name-friendly characters with an underscore.
  host="${host//./_}"

  if [[ -n "${1:-}" ]]; then
    local session="${1#$host-}"
    host_session="${host}-${session}"
  else
    # If no session name given, make one up. Use the smallest free integer
    # available.
    host_session="$(get_default_session_id "${host}")"
  fi

  # Create a new raw tmux session. Attach to an existing session
  # of the same name if it is already running.
  tmux new-session -A -s "${host_session}"
}

main "$@"
