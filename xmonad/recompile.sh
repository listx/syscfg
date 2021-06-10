#!/bin/sh

set -ex

xmonad_dir="${HOME}/syscfg/xmonad"
cd "${xmonad_dir}"
nix-shell --command "cabal install --installdir ${xmonad_dir} --overwrite-policy=always"
cp -f "${xmonad_dir}/xmonad" "${xmonad_dir}/xmonad-x86_64-linux"

