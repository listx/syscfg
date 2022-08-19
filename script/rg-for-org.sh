#!/usr/bin/env bash

# Usage: $0 <DIR> <PATTERN>
#
# Search with ripgrep. <PATTERN> is passed directly to rg as-is.

set -euo pipefail

main()
{
    local dir
    local pattern

    dir="${1}"
    pattern="${2}"

    shift 2

    local pattern

    declare -a args
    # Enable negative lookahead.
    args+=(--pcre2)
    # Show line numbers.
    args+=(--line-number)
    # Terminate filename with NUL byte.
    args+=(--null)
    # Disable buffering (recommended for scripts).
    args+=(--line-buffered)
    # Do not group matches by each file.
    args+=(--no-heading)
    # Do not colorize.
    args+=(--color=never)
    # Always use "/" as path separator.
    args+=(--path-separator /)
    # Ignore case sensitivity if all characters in the pattern are lowercase.
    if ! [[ "${pattern}" =~ [[:upper:]] ]]; then
        args+=("--ignore-case")
    fi

    for arg in "$@"; do
        args+=("${arg}")
    done

    rg "${args[@]}" -e "${pattern}" "${dir}"
}

main "$@"
