#!/usr/bin/env bash

# We only need to run this script when we add a new terminal color theme.

set -euo pipefail

root_dir="${HOME}/syscfg/script/terminal-themes"

# Input is a string like "#abcdef". We have to convert this to: "#xabab #xcdcd
# #xefef".
__emacs_color(){

    local color
    local r
    local g
    local b
    color=${1}

    # Return #000000 if we are told to parse an invalid color.
    if [[ ! "${color}" =~ ^#[0-9a-f]+$ ]] || (( ${#color} != 7 )); then
        echo "#x0000 #x0000 #x0000"
        return
    fi

    color=${color#\#}
    r=${color:0:2}
    g=${color:2:2}
    b=${color:4:2}

    echo "#x${r}${r} #x${g}${g} #x${b}${b}"
}

set_theme() {
    local theme
    local for_tmux
    local renderer_path
    local root_dir
    local emacs_theme
    local all_colors
    local extra_colors
    local colorKey
    local colorValue
    declare -A colors
    local __elisp

    theme="${1}"
    root_dir="${HOME}/syscfg/script/terminal-themes"
    renderer_path="${root_dir}/renderControlSequences.dhall"

    # Change doom emacs color theme. This doesn't check if the emacs daemon is
    # already running --- rather it just executes and forgets about it.
    emacs_theme="$(dhall text <<< "(${root_dir}/themes/${theme}).emacsTheme")"

    mapfile -t all_colors < <(dhall text <<< "${root_dir}/listColorsForShell.dhall ${root_dir}/themes/${theme}")
    extra_colors="$(dhall text <<< "${root_dir}/listColorsForEmacs.dhall ${root_dir}/themes/${theme}")"
    for colorline in "${all_colors[@]}"; do
        colorKey=${colorline%=*}
        colorValue=${colorline#*=}
        colors[${colorKey}]=${colorValue}
    done

    __elisp=$(cat << EOF
    (progn
        ; Set custom colors. These are only usable from 24-bit color-enabled
        ; emacs.
${extra_colors}

        ; Set 16 colors. This is because the default 16 colors are hardcoded by
        ; emacs, making it impossible to refer to the colors used by the
        ; existing terminal color theme.
        (tty-color-define "black"          0 '($(__emacs_color "${colors[black]}")))
        (tty-color-define "red"            1 '($(__emacs_color "${colors[red]}")))
        (tty-color-define "green"          2 '($(__emacs_color "${colors[green]}")))
        (tty-color-define "yellow"         3 '($(__emacs_color "${colors[yellow]}")))
        (tty-color-define "blue"           4 '($(__emacs_color "${colors[blue]}")))
        (tty-color-define "magenta"        5 '($(__emacs_color "${colors[magenta]}")))
        (tty-color-define "cyan"           6 '($(__emacs_color "${colors[cyan]}")))
        (tty-color-define "white"          7 '($(__emacs_color "${colors[white]}")))
        (tty-color-define "brightblack"    8 '($(__emacs_color "${colors[brightblack]}")))
        (tty-color-define "brightred"      9 '($(__emacs_color "${colors[brightred]}")))
        (tty-color-define "brightgreen"   10 '($(__emacs_color "${colors[brightgreen]}")))
        (tty-color-define "brightyellow"  11 '($(__emacs_color "${colors[brightyellow]}")))
        (tty-color-define "brightblue"    12 '($(__emacs_color "${colors[brightblue]}")))
        (tty-color-define "brightmagenta" 13 '($(__emacs_color "${colors[brightmagenta]}")))
        (tty-color-define "brightcyan"    14 '($(__emacs_color "${colors[brightcyan]}")))
        (tty-color-define "brightwhite"   15 '($(__emacs_color "${colors[brightwhite]}")))
        (load-theme '${emacs_theme} t)
        (l/reset-faces)
    )
EOF
    )
    >/dev/null 2>&1 emacsclient -e "${__elisp}" -a "" & disown

    # Print control sequences. These are meant to be sourced by the caller.
    for_tmux="False"
    if [[ -n "${TMUX:-}" ]]; then
        for_tmux="True"
    fi
    echo "${renderer_path} ${for_tmux} ${root_dir}/themes/${theme}" | dhall text
    echo "export TERM_COLOR_THEME=${theme}"
}

cycle_theme()
{
    local root_dir
    local direction
    local i
    local j
    local len
    local next_theme

    root_dir="$HOME/syscfg/script/terminal-themes/themes"
    direction=$1

    # Cycle through to the next theme in the list.
    declare -A themes
    declare -A themes_indexed
    i=0
    len=0
    for theme in "${root_dir}"/*.dhall; do
        t=$(basename "$theme")
        themes[$i]="$t"
        themes_indexed["$t"]=$i
        ((i+=1))
        ((len+=1))
    done
    j=${themes_indexed["${TERM_COLOR_THEME}"]}
    if [[ "$direction" == "next" ]]; then
        ((j+=1))
    else
        ((j+=len - 1))
    fi
    next_theme=${themes[$((j%len))]}
    set_theme "${next_theme}"
}

main() {
    arg="${1:-next}"
    case "${arg}" in
        *.dhall) set_theme "${arg}" ;;
        next|prev) cycle_theme "${arg}" ;;
        *)
            echo >&2 "unrecognized argument ${arg}"
            return 1
            ;;
    esac
}

main "$@"
