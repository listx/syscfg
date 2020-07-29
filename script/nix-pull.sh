#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

nix_pull()
{
    local from_host
    local closure
    from_host="${1}"
    closure="${2}"
    nix-copy-closure --from "${from_host}" "${closure}"
    nix-env --install "${closure}"
}

from_host="${1}"
shift

for arg; do
    nix_pull "${from_host}" "${arg}"
done

