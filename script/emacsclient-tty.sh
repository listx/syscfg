#!/usr/bin/env bash

set -euo pipefail

elisp=$(cat << EOF
(progn
  ; Open up the git buffer.
  (find-file "$(readlink -e "${1}")")
  ; Set line length to 72, because commit message hygiene matters.
  (setq fill-column 72)
  ; Go to the top of the buffer.
  (beginning-of-buffer)
)
EOF
)

# The (find-file ...) avoids showing "*scratch*" buffer on startup when invoking
# from emacsclient.
emacsclient \
    --alternate-editor "" \
    --tty \
    --eval "${elisp}"
