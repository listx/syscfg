#!/usr/bin/env bash

# We only need to run this script when we add a new terminal color theme.

set -euxo pipefail

root_dir="${HOME}/syscfg/script/terminal-themes"

# Convert dhall-ized color definitions into something Alacritty can understand.
get_theme_alacritty() {
    local renderer_path
    local theme="${1:-}"

    renderer_path="${root_dir}/renderAlacrittyColorYaml.dhall"

    dhall text <<< "${renderer_path} ${theme}"
}

main() {
    for theme in "${root_dir}"/themes/*; do
        output_path="${root_dir}/../../alacritty/colors_$(basename "${theme}")"
        output_path="${output_path%.dhall}.yml"
        get_theme_alacritty "${theme}" > "${output_path}"
    done
}

main "$@"
