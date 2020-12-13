#!/usr/bin/env bash

set -euo pipefail

set_elisp()
{
    local buffer_filename
    local maybe_fill_72=""

    buffer_filename="$(readlink -e "${1}")"

    # If Git is invoking this editor (for `git commit'), then manually set the line
    # length to 72.
    case "${buffer_filename}" in
        *COMMIT_EDITMSG|*git-rebase-todo)
            maybe_fill_72="(setq fill-column 72)"
        ;;
    esac

    __elisp=$(cat << EOF
    (progn
      ; Open up the git buffer.
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
}

main()
{
    set_elisp "$1"

    # The (find-file ...) avoids showing "*scratch*" buffer on startup when
    # invoking from emacsclient.
    emacsclient \
        --alternate-editor "" \
        --tty \
        --suppress-output \
        --eval "${__elisp}"
}

main "$@"
