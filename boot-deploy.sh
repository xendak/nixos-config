#!/usr/bin/env bash

ff=/home/flakes/.mozilla/firefox/Snow/search.json.mozlz4
cc=/home/flakes/.config/chromium/Default/Preferences
cl="/home/flakes/.config/chromium/Local State"
[[ -f "$ff" ]] && rm $ff 
[[ -f "$cc" ]] && rm $cc 
[[ -f "$cl" ]] && rm "$cl" 
cd $HOME/Flake

sudo nixos-rebuild boot --flake .#$USER
