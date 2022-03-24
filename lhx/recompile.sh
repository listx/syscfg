#!/bin/sh

set -ex

lhx_dir="${HOME}/syscfg/lhx"
cd "${lhx_dir}"
nix-shell --command "cabal install --installdir ${lhx_dir} --overwrite-policy=always"
