#!/usr/bin/env bash

cd "$HOME/Flake" || exit

to_del=(
"$HOME/.config/gtk-2.0/gtkrc"
"$HOME/.config/gtk-3.0/settings.ini"
"$HOME/.config/gtk-4.0/gtk.css"
"$HOME/.config/gtk-4.0/settings.ini"
"$HOME/.config/zathura/zathurarc"
)

for fs in "${to_del[@]}"; do
 [[ -f "$fs" ]] && rm "$fs"
done

sudo nixos-rebuild switch --flake .#"$USER" --show-trace
# sudo nixos-rebuild switch --flake .#"$USER" --option eval-cache false --show-trace
