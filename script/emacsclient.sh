#!/usr/bin/env bash

set -euo pipefail

emacsclient --alternate-editor "" --create-frame "$@"
