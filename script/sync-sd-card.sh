#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if (( $# != 1 )) || [[ -z $1 ]]; then
	echo >&2 "usage: $0 <FUJI_FOLDER_PREFIX>"
fi

if [[ -d /Volumes/sd128GB ]]; then
	rsync -ahP --no-whole-file --inplace \
		--exclude='.*/' \
		--exclude='*.DAT' \
		/Volumes/{sd128GB,wd1TB}
fi

if [[ -d /Volumes/wd1TB ]]; then
	mapfile -t raws < <(find "/Volumes/wd1TB/sd128GB/DCIM/${1}_FUJI" -name '*.RAF' | sort)
	if ! (( ${#raws[@]} )); then
		echo >&2 "no *.RAF raws found in ${1}_FUJI"
		exit
	fi
	parallel -j "$(nproc --all)" -m raw-to-jpg.sh \
		~/converted \
		::: "${raws[@]}"
fi
