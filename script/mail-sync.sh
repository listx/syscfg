#!/usr/bin/env bash
#
# Call lieer to sync notmuch database with Gmail.

set -euo pipefail

# Be explicit about which Notmuch configuration we're using.
export NOTMUCH_CONFIG="${HOME}/.notmuch-config"

# If we're running inside cron, we might not be aware of the usual paths used by
# Nixpkgs. Enable them here.
export PATH="${HOME}/.nix-profile/bin:/run/current-system/sw/bin:${PATH}"

NOTMUCH_TAGS_FILE="${HOME}/syscfg/notmuch/tags"

main()
{
    cd ~/mail/linusarver@gmail.com

    # "gmi" is a script provided by the "lieer" package. It fetches mail from
    # Gmail to disk.
    gmi sync

    # Re-index new emails.
    notmuch new

    # Tag emails.
    notmuch tag --batch --input="${NOTMUCH_TAGS_FILE}"
}

main "$@"
