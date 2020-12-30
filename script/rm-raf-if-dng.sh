#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if (( $# != 1 )); then
	echo >&2 "usage: $0 <DIRECTORY>"
fi

if [[ -z $1 ]]; then
	echo >&2 "usage: $0 <DIRECTORY>"
fi

mapfile -t raws < <(find "$1" -type f -name '*.RAF' | sort)

for raw in "${raws[@]}"; do
	# Delete *.RAF file if a corresponding *.dng file exists.
	if [[ -f "${raw/RAF/dng}" ]]; then
		rm -fv "${raw}"
	fi
done

echo DONE
