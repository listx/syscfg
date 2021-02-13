#!/usr/bin/env bash

# This invokes emacs and makes it export the agenda to disk. The details are in
# the `l/export-agenda' function in the emacs config.

set -o errexit
set -o nounset
set -o pipefail

~/.nix-profile/bin/emacs \
	--batch \
	--load ~/.emacs.d/init.el \
	--eval '(l/export-agenda)'
