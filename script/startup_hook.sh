#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# Start up various background services.

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
