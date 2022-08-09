#!/usr/bin/env bash

set -euo pipefail

mkdir -p ~/.config/systemd/user
pushd ~/.config/systemd/user
ln -sf ~/syscfg/lh/lh.service

systemctl --user daemon-reload
systemctl --user start lh.service
