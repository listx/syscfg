#!/usr/bin/env bash

nix-channel --add https://github.com/oxalica/rust-overlay/archive/master.tar.gz rust-overlay
nix-channel --update
