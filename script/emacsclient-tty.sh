#!/usr/bin/env bash

set -euo pipefail

set_elisp()
{
    local file="${1}"
    local buffer_filename
    local maybe_fill_72=""

    buffer_filename="$(readlink -e "${file}")"

    # If Git is invoking this editor (for `git commit'), then manually set the line
    # length to 72.
    case "${buffer_filename}" in
        *COMMIT_EDITMSG|*git-rebase-todo)
            maybe_fill_72="(setq fill-column 72)"
        ;;
    esac

    # Exit if we can't determine the $buffer_filename, because it is required.
    if [[ -z "${buffer_filename}" || ! -e "${buffer_filename}" ]]; then
        echo >&2 "could not determine \$buffer_filename; aborting"
        exit 1
    fi

    __elisp=$(cat << EOF
    (prog1
      ; Open the file.
      (find-file "${buffer_filename}")
      ${maybe_fill_72}
      ; Disable menu (for some reason the menu is enabled on the frame
      ; sometimes).
      (menu-bar-mode 0)
      ; Go to the top of the buffer.
      (beginning-of-buffer)
    )
EOF
    )

    # Strip comments.
    __elisp="$(echo -e "${__elisp}" | sed '/^\s\+\?;/d;s/;.\+//')"
}

main()
{
    set_elisp "$1"

    # The (find-file ...) avoids showing "*scratch*" buffer on startup when
    # invoking from emacsclient.
    emacsclient \
        --alternate-editor "" \
        --tty \
        --eval "${__elisp}"
}

main "$@"
