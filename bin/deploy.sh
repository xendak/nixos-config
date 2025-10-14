#!/usr/bin/env bash

cd "$HOME/Flake" || exit



to_del=(
"/home/drops/.config/gtk-2.0/gtkrc"
"/home/drops/.config/gtk-3.0/settings.ini"
"/home/drops/.config/gtk-4.0/gtk.css"
"/home/drops/.config/gtk-4.0/settings.ini"
"/home/drops/.config/zathura/zathurarc"
)

for fs in "${to_del[@]}"; do
 [[ -f "$fs" ]] && rm "$fs"
done

#sudo nixos-rebuild switch --flake .#$USER --show-trace
sudo nixos-rebuild switch --flake .#"$USER" --show-trace
# sudo nixos-rebuild switch --flake .#"$USER" --option eval-cache false --show-trace
