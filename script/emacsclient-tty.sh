#!/usr/bin/env zsh

read -r -d '' elisp << EOF
(let
  (
  )
  (find-file-other-tab "$1")
  (setq fill-column 72)
  (beginning-of-buffer)
)
EOF

# The (find-file ...) avoids showing "*scratch*" buffer on startup when invoking
# from emacsclient.
emacsclient --alternate-editor "" --tty \
	--eval "${elisp}"
