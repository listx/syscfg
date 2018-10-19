#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

if [[ -d /Volumes/sd128GB ]]; then
    rsync -ahP --no-whole-file --inplace /Volumes/{sd128GB,wd1TB}
fi

if [[ -d /Volumes/wd1TB ]]; then
    raws=($(find /Volumes/wd1TB/sd128GB -name '*.RAF' | sort))
    raw-to-jpg.sh \
        ~/converted \
        "${raws[@]}"
fi
