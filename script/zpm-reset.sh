#!/usr/bin/env bash

# Usage: $0
#
# Reset zpm (typically needed after messing around with ~/.zshrc). Needed
# especially on Mac in practice for some reason.

set -o errexit
set -o nounset
set -o pipefail

rm -rf "${TMPDIR:-/tmp}/zsh-${UID:-user}"
rm ~/.zshrc.zwc
rm ~/.compdump
rm ~/.zpm/plugins/**/*.zwc
