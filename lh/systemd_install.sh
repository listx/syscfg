#!/usr/bin/env bash

set -euo pipefail

systemctl --user daemon-reload
systemctl --user start lh.service
