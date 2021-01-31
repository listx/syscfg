#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# Get *.jpg files in foo/, and list *.dng files in bar/. List all *.dng files
# that are in bar (matching filename without the extension) that are not in
# foo/.
#
# Usage: $0 <JPG_FOLDER> <DNG_FOLDER> | xargs rm

if (( $# != 2 )) || [[ -z $1 ]] || [[ -z $2 ]]; then
	echo >&2 "usage: $0 <JPG_FOLDER> <DNG_FOLDER>"
fi

jpg_dir=$1
dng_dir=$2

mapfile -t dngs < <(find "${dng_dir}" -name '*.dng' | sort)

for dng in "${dng_dir}"/*.dng; do
	photo="$(basename "${dng%.dng}")"
	hi_res="${dng}"
	lo_res="${jpg_dir}/${photo}.jpg"
	# If the hi-res photo does not have a lo-res counterpart, then it is a
	# candidate for deletion. The idea is that we would have gone through the
	# jpgs to delete those jpgs we were not interested in.
	if [[ ! -f "${lo_res}" ]]; then
		echo "${hi_res}"
	fi
done
