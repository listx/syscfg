#!/usr/bin/env bash

# Extract audio files from video files and save them as .ogg, recursively.

set -o errexit
set -o nounset
set -o pipefail

for f in **/*; do
    echo
    echo "Processing ${f}"

    if ! ffprobe -i "${f}" -show_streams -select_streams a -loglevel error 2>/dev/null 1>&2; then
        echo >&2 "Skipping ${f} because it is not a video file."
        continue
    fi

    ffmpeg -i "${f}" -vn -y "${f}.ogg"
done
