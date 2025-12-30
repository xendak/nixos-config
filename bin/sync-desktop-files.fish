#!/usr/bin/env fish

set desktop_dir $HOME/Flake/home/common/desktop
set applications_dir $HOME/.local/share/applications

for f in (eza $desktop_dir | rg ".desktop")
    ln -sf "$desktop_dir/$f" "$applications_dir/$f"
end
