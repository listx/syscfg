#!/usr/bin/env zsh

read -r -d '' elisp << EOF
(let
  (
  )
  (find-file "$1")
  (setq fill-column 72)
  (beginning-of-buffer)
  (load "elscreen" "ELScreen" t)
)
EOF

# The (find-file ...) avoids showing "*scratch*" buffer on startup when invoking
# from emacsclient.
emacsclient --alternate-editor "" --tty \
	--eval "${elisp}"
