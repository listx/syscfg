#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

pane_pwd_same()
{
  local window_id
  local pane_id
  local has_focus

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
  pwd_shortened="$(~/.cargo/bin/lhc path-shorten "${pwd_new}" || ~/syscfg/lhc/doc/code/simplify_path.sh "${pwd_new}")"
  echo $pwd_shortened

  # TODO: Garbage-collect these evironment variables when the window is closed.
  tmux set-environment "L_TMUX_PANE_PWD_${window_id}" "${pwd_new};${pwd_shortened}"
}

main "$@"
