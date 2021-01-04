#!/usr/bin/env bash

# Extract audio files from video files and save them as .ogg, recursively.

set -o errexit
set -o nounset
set -o pipefail

shopt -s globstar

# Collect video files to process.
videos=()
for f in **/*; do
    echo
    echo "Processing ${f}"

    if ! ffprobe -i "${f}" -show_streams -select_streams v 2>/dev/null | grep codec_type=video; then
        echo >&2 "Skipping ${f} because it is not a video file."
        continue
    fi

    if ! ffprobe -i "${f}" -show_streams -select_streams a 2>/dev/null | grep codec_type=audio; then
        echo >&2 "Skipping ${f} because it does not have an audio stream."
        continue
    fi

    if [[ -f "${f}.ogg" ]]; then
        echo >&2 "Skipping ${f} because it already has an audio equivalent."
        continue
    fi

    videos+=("${f}")
done

parallel -j "$(nproc)" < <(for f in "${videos[@]}"; do echo ffmpeg -i "\"${f}\"" -vn -y "\"${f}.ogg\""; done)
