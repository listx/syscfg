#!/usr/bin/env bash

# Launch emacs. Meant to be used for opening up an org-roam buffer from outside
# of emacs (killer use case is for the browser when using org-roam-server-mode).

~/.nix-profile/bin/emacsclient --no-wait "${1}"
