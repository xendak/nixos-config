#!/usr/bin/env bash

ff=$HOME/.mozilla/firefox/Snow/search.json.mozlz4
cc=$HOME/.config/chromium/Default/Preferences
cl="$HOME/.config/chromium/Local State"
[[ -f "$ff" ]] && rm $ff 
[[ -f "$cc" ]] && rm $cc 
[[ -f "$cl" ]] && rm "$cl" 
cd $HOME/Flake
sudo nixos-rebuild switch --flake .#$USER --show-trace
#sudo nixos-rebuild switch --flake .#$USER --option eval-cache false
