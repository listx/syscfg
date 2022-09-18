#!/usr/bin/env bash

# Usage: $0 <DIR> <MODE> <PATTERN> [<MULTILINE_PATTERN>] [OPTS...]
#
# Search with ripgrep. MODE can be either "all" or "regions". If "regions" is
# given, the DIR is searched for text regions defined in MULTILINE_PATTERN, and
# these regions are in turn searched by PATTERN. If MODE is "all", we do a plain
# ripgrep search for PATTERN.
#
# For "region" mode, we call rg twice, first to find multiline matches and then
# a second time to filter within those matches.  This is useful for searching
# source code blocks in org-mode files, which can potentially have hundreds of
# lines in them.
#
# The [OPTS...] is treated specially. Namely, it's difficult to tell which
# options are meant for the first invocation of rg vs the second. So each
# argument's first character determines where it should go (and is removed
# before being passed to rg).

exec 2>&1
set -euo pipefail

main()
{
    local mode

    dir="${1}"
    mode="${2}"

    if [[ "${mode:-}" == "regions" ]]; then
        pattern="${3}"
        pattern_multiline="${4}"
        shift 4
        search_regions "$@"
    else
        pattern="${3}"
        shift 3
        search_all "$@"
    fi
}

search_regions()
{
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

search_all()
{
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
