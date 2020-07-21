#!/bin/bash

# Convert to jpeg.

# Output directory (remove any trailing slash).
outdir=${1%%/}
shift

inputs=($@)
for input in "${inputs[@]}"; do
    # Input file: isolate filename (strip leading directories).
    infile="${input##*/}"
    # Output file: rename to .jpg extension.
    outfile="${infile%.*}.jpg"

    output="${outdir}/${outfile}"
    if [[ -f "${output}" ]]; then
        continue
    fi
    sips -s format jpeg $input --out "${output}"
done
