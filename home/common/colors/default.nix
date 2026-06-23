{
  config,
  host,
  lib,
  inputs,
  ...
}:
{
  imports = [
    inputs.vynta.homeManagerModules.default
    inputs.vynta.homeManagerModules.targets
  ];

  themes = {
    enable = true;
    theme = "gorgoroth";
    wallpaper = "${config.home.homeDirectory}/Flake/home/common/wallpapers/13.jpg";
    stateDir = "${config.home.homeDirectory}/Flake";

    wallpaperScript = [
      "fish"
      "${config.home.homeDirectory}/Flake/home/common/programs/quickshell/niri/wallpaper.fish"
      "-f"
    ];

    palettes = inputs.vynta.builtinPalettes // { };

    # vynta-start
    starterExtraCommands =
      # bash
      ''
        [ -d "$HOME/Desktop"   ] && rmdir "$HOME/Desktop"   2>/dev/null
        [ -d "$HOME/Templates" ] && rmdir "$HOME/Templates" 2>/dev/null
        [ -d "$HOME/Public"    ] && rmdir "$HOME/Public"    2>/dev/null
        [ -d "$HOME/.cache"    ] && rm -rf "$HOME/.cache"   2>/dev/null
        [ -d "$HOME/Projects"  ] && rmdir "$HOME/Projects"  2>/dev/null
        [ -d "$HOME/tmp/Screenshots" ] || mkdir -p "$HOME/tmp/Screenshots" 2>/dev/null
      ''
      +
        lib.optionalString (host != "Rain")
          # bash
          ''
            pkill -x "quickshell" || true
            sleep 0.2
            qs -d -c "${config.home.homeDirectory}/Flake/home/common/programs/quickshell/niri/"
          ''
      +
        lib.optionalString (host == "Rain")
          # bash
          ''
            cp -r "/persist/home/${config.home.username}/Flake/home/mac/eww/elena-fonts" "/home/${config.home.username}/.local/share/fonts"
            cp -r "/persist/home/${config.home.username}/Flake/home/mac/eww" "/home/${config.home.username}/.config/eww"
            sh "/home/${config.home.username}/Flake/bin/battery-monitor" &
          '';

    # GTK / QT
    targets-gtk.enable = true;
    targets-qt.enable = true;

    # Editors
    targets-helix.enable = true;
    targets-emacs.enable = true;
    targets-nvim.enable = true;
    targets-kak.enable = true;

    # vaultDir defaults to ~/Documents/Notes/<username>
    targets-obsidian.enable = true;

    # Terminals
    targets-foot.enable = true;
    targets-zellij.enable = true;
    targets-wezterm.enable = true;

    # Term Utils
    targets-yazi.enable = true;
    targets-zathura.enable = true;
    targets-lazygit.enable = true;
    targets-fzf.enable = true;
    targets-rofi.enable = true;
    targets-nushell = {
      enable = true;
      file = "${config.home.homeDirectory}/Flake/home/common/programs/terminal/nushell/colors.nu";
    };

    # Discord
    targets-vesktop.enable = true;
    targets-discordcanary.enable = true;

    # Desktop
    targets-niri.enable = true;
    targets-caelestia.enable = true;
    targets-fcitx5.enable = true;

    # Browser
    # profileDir defaults to ~/.config/zen/<username>
    targets-zen.enable = true;

    # :Rain
    targets-dunst.enable = host == "Rain";
    targets-eww.enable = host == "Rain";
  };
}
