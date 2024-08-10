#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# Start up various background services. This script is expected to be called
# from XMonad on startup. The advantage of using this script is that we don't
# have to recompile XMonad every time we want to change something here (as this
# script is just Bash, not Haskell).

case $(hostname) in
k0)
	# Start up `hledger web` instances over ports 8000 and 8001.
	~/syscfg/script/hledger.sh &
	# Start up a simple server to serve ~/agenda.html.
	~/syscfg/script/serve-org-agenda.py &
	# Start mpd.
	mpd &
;;
esac
