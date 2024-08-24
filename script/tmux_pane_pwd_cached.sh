#!/usr/bin/env bash
#
# Script used by tmux to figure out how to display the status bar for the
# current window. Note that this is different from setting the window name (aka
# "#{window_name}", which we purposely avoid due to race conditions.
#
# The point of this script is to avoid spamming melby with too many requests.
# Instead, we only ask melby to shorten a path for us if we detect that there is a
# change in the current path.

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

pane_pwd_same()
{
  local window_id
  local pane_id

  local pwd_new

  window_id="${1}"
  pane_id="${2}"
  pwd_new="${3}"

  # Format is L_TMUX_PANE_PWD_<WINDOW_ID>=<PWD_LONG>\n<PWD_SHORT>
  __pwd_old="$(tmux show-environment L_TMUX_PANE_PWD_${window_id})"
  __pwd_old_short="${__pwd_old}"
  __pwd_old="${__pwd_old#*=}"
  __pwd_old="${__pwd_old%;*}"

  __pwd_old_short="${__pwd_old_short#*;}"

  if [[ "${pwd_new}" == "${__pwd_old}" ]]; then
    return 0
  fi

  return 1
}

main()
{
  local window_id
  local pane_id
  local pwd_new
  local pwd_shortened

  window_id="${1}"
  # Get rid of leading "@" symbol, as it is unnecessary.
  window_id="${window_id#@}"
  pane_id="${2}"
  pwd_new="${3}"

  # If there is no change to the active pane's pwd, then there's nothing to do;
  # just reuse the existing window name as-is.
  if pane_pwd_same "${window_id}" "${pane_id}" "${pwd_new}" && [[ -n "${__pwd_old_short}" ]]; then
    # window_name_old="${tmux display-message -p -t "${pane_id}" '#{window_name}'}"
    # if [[ "${window_name_old:0:1}" != "<" ]]; then
    #   echo "${window_name_old}"
    #   return
    # fi
    echo "${__pwd_old_short}"
    return
  fi

  # If the active pane's pwd did change, then we must use a newly shortened
  # window name.
  pwd_shortened="$(~/syscfg/script/simplify_path.sh "${pwd_new}")"
  echo $pwd_shortened

  # TODO: Garbage-collect these tmux environment variables when the window is
  # closed.
  #
  # Unfortunately as of tmux 3.2a (Nov 2021) there is no way to set a hook (with
  # set-hook) that runs just before a window is closed. So technically this is a
  # memory leak but it is OK because:
  #
  #   (1) we don't create that many windows (maybe a few hundred, if we have tmux running for months on end);
  #   (2) the rate of the leak is negligible due to (1); and
  #   (3) tmux already garbage-collects these variables when the session is
  #   closed.
  #
  # But this is worth revisiting in the future.
  tmux set-environment "L_TMUX_PANE_PWD_${window_id}" "${pwd_new};${pwd_shortened}"
}

main "$@"
