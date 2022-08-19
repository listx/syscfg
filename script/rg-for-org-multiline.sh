#!/usr/bin/env bash

# Usage: $0 <DIR> <MULTILINE_PATTERN> <PATTERN> [OPTS...]
#
# Search with ripgrep for multiline strings. Namely, we call rg twice, first to
# find multiline matches and then a second time to filter within those matches.
# This is because sometimes we have too many lines from a single match. This is
# used for searching source code blocks in org-mode files, which can potentially
# have hundreds of lines in them.
#
# The [OPTS...] is treated specially. Namely, it's difficult to tell which
# options are meant for the first invocation of rg vs the second. So each
# argument's first character determines where it should go (and is removed
# before being passed to rg).

exec 2>&1
set -euo pipefail

main()
{
    local dir
    local pattern_multiline
    local pattern

    dir="${1}"
    pattern_multiline="${2}"
    pattern="${3}"

    shift 3

    declare -a args_multiline
    declare -a args
    # Enable negative lookahead.
    args_multiline+=(--pcre2)
    # Show line numbers.
    args_multiline+=(--line-number)
    # Terminate filename with NUL byte.
    args_multiline+=(--null)
    # Disable buffering (recommended for scripts).
    args_multiline+=(--line-buffered)
    # Do not group matches by each file.
    args_multiline+=(--no-heading)
    # Do not colorize.
    args_multiline+=(--color=never)
    # Always use "/" as path separator.
    args_multiline+=(--path-separator /)
    # Turn on multiline search.
    args_multiline+=(--multiline)
    # Make "." match newlines.
    args_multiline+=(--multiline-dotall)

    args+=(--pcre2)
    args+=(--line-buffered)
    args+=(--no-heading)
    args+=(--color=never)
    # Treat the input as text, even if there are NUL bytes (NUL byte presence is
    # used as a heuristic by rg to determine if the input is a binary stream).
    # Otherwise the second rg invocation will think that the input is a binary
    # stream and not work the way we expect it to.
    args+=(--text)
    # Ignore case sensitivity if all characters in the pattern are lowercase.
    if ! [[ "${pattern}" =~ [[:upper:]] ]]; then
        args+=("--ignore-case")
    fi

    # Process [OPTS...].
    for arg in "$@"; do
        case "${arg}" in
            1*) args_multiline+=("${arg#1}") ;;
            2*) args+=("${arg#2}") ;;
        esac
    done

    rg "${args_multiline[@]}" -e "${pattern_multiline}" "${dir}" | rg "${args[@]}" -e "${pattern}"
}

main "$@"
