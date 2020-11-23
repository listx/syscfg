#!/usr/bin/env bash

# Usage: hledger.sh
#
# This runs 2 instances of `hledger-web`.

set -o errexit
set -o nounset
set -o pipefail

get_ip_addr()
{
    case "$(hostname)" in
	k0)
		echo 192.168.0.4 ;;
	*)
		echo 127.0.0.1 ;;
	esac
}

2>&1 >/dev/null hledger -R web -- --serve --host=$(get_ip_addr) --port=8000 --base-url=http://$(hostname):8000 & disown
2>&1 >/dev/null hledger    web -- --serve --host=$(get_ip_addr) --port=8001 --base-url=http://$(hostname):8001 & disown
