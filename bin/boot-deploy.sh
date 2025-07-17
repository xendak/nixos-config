#!/usr/bin/env bash

cd $HOME/Flake

sudo nixos-rebuild boot --flake .#$USER --show-trace --log-format internal-json -v |& nom --json
