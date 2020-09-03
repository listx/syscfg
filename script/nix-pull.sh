#!/usr/bin/env bash

# Usage: nix-pull.sh <HOST>
#
# This clones all "nix-env" packages in the remote host into the current
# system.

set -o errexit
set -o nounset
set -o pipefail

nix_pull()
{
    local remote
    local closure
    remote="${1}"
    closure="${2}"
    nix-copy-closure --from "${remote}" "${closure}"
    nix-env --install "${closure}"
}

remote="${1}"
shift

for closure in $(ssh ${USER}@${remote} nix-env -q --out-path | awk '{print $2}'); do
    nix_pull "${remote}" "${closure}"
done

