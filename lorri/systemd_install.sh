#!/usr/bin/env bash

# This is needed for non-NixOS Linux systems, because in NixOS the lorri daemon
# is available as a declarative service with the "services.lorri.enable"
# option. See
# https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=lorri.

set -euo pipefail

systemctl --user daemon-reload
systemctl --user enable --now lorri.socket
systemctl --user start lorri.socket
