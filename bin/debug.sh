#!/usr/bin/env bash

cd "$HOME/Flake" || exit

NIX_ABORT_ON_WARN=1 sudo nixos-rebuild boot --flake .#"$USER" --show-trace --impure --log-format internal-json -v |& nom --json

