{
  pkgs,
  inputs,
  config,
  ...
}:
let
  theme-switcher = (
    pkgs.writeShellScriptBin "theme-switcher" ''
      HELIX_THEME_DIR="$HOME/.config/helix/themes"
      WEZTERM_THEME_DIR="$HOME/.config/wezterm/colors"
      # QUICKSHELL_THEME_DIR="$HOME/Flake/home/common/programs/quickshell/themes"
      QUICKSHELL_CURRENT_THEME_DIR="$HOME/.local/state/caelestia/scheme"
      THEME_DIR="$HOME/.config/themes"

      case $1 in
        "dark")
          NEW_FILE="dark"
          GTK_INI="dark"
          export GTK_THEME="${config.gtk.theme.name}:dark"
          gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
          dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
        ;;
        "light")
          NEW_FILE="light"
          GTK_INI="light"
          export GTK_THEME="${config.gtk.theme.name}"
          # TODO: check if its "default" instead of prefer-light
          gsettings set org.gnome.desktop.interface color-scheme "prefer-light"
          dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'"
        ;;
        *)
          NEW_FILE="default"
          GTK_INI="dark"
          export GTK_THEME="${config.gtk.theme.name}:dark"
          dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
          gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
        ;;
      esac


      mkdir -p "$HOME/.config/gtk-2.0"
      rm -f "$HOME/.config/gtk-2.0/gtkrc"
      mkdir -p "$HOME/.config/gtk-3.0"
      rm -f "$HOME/.config/gtk-3.0/settings.ini"
      mkdir -p "$HOME/.config/gtk-4.0"
      # rm -rf "$HOME/.config/gtk-4.0/assets"
      rm -f "$HOME/.config/gtk-4.0/gtk.css"
      rm -f "$HOME/.config/gtk-4.0/gtk-dark.css"
      rm -f "$HOME/.config/gtk-4.0/settings.ini"
      rm -f "$HOME/.config/zathura/zathurarc"
      mkdir -p "$HOME/.config/qt5ct"
      mkdir -p "$HOME/.config/qt6ct"
      cat "$THEME_DIR/gtk-$GTK_INI.ini" >"$HOME/.config/gtk-2.0/gtkrc"
      cat "$THEME_DIR/gtk-$GTK_INI.ini" >"$HOME/.config/gtk-3.0/settings.ini"
      cat "$THEME_DIR/gtk-$GTK_INI.ini" >"$HOME/.config/gtk-4.0/settings.ini"
      cat "$THEME_DIR/qt-$NEW_FILE.conf" >"$HOME/.config/qt5ct/qt5ct.conf"
      cat "$THEME_DIR/qt-$NEW_FILE.conf" >"$HOME/.config/qt6ct/qt6ct.conf"
      cat "$THEME_DIR/kdeglobals-$NEW_FILE" >"$HOME/.config/kdeglobals"
      cat "$THEME_DIR/zathura-$NEW_FILE" >"$HOME/.config/zathura/zathurarc"

      cat "$THEME_DIR/gtk-$NEW_FILE.css" > "$HOME/.config/gtk-4.0/gtk.css" 
      cat "$THEME_DIR/gtk-$NEW_FILE.css" > "$HOME/.config/gtk-4.0/gtk-dark.css" 
      cat "$THEME_DIR/gtk-$NEW_FILE.css" > "$HOME/.config/gtk-3.0/gtk.css" 
      cat "$THEME_DIR/gtk-$NEW_FILE.css" > "$HOME/.config/gtk-3.0/gtk-dark.css" 


      cat "$HELIX_THEME_DIR/$NEW_FILE.toml" >"$HELIX_THEME_DIR/current.toml"
      sed -i "s/##/#/g" "$HELIX_THEME_DIR/current.toml"

      cat "$WEZTERM_THEME_DIR/$NEW_FILE.lua" >"$WEZTERM_THEME_DIR/current.lua"
      sed -i "s/##/#/g" "$WEZTERM_THEME_DIR/current.lua"

      mkdir -p $QUICKSHELL_CURRENT_THEME_DIR
      cat "$THEME_DIR/quickshell-$NEW_FILE.txt" >"$QUICKSHELL_CURRENT_THEME_DIR/current.txt"

      echo $(date +"%d/%m/%y | %H:%M >") "Theme switched to $NEW_FILE." >> /tmp/theme-switcher
      pkill -USR1 hx
      notify-send "Theme Manager" --expire-time=2000 --app-name="Theme Manager" --icon=preferences-desktop-theme "Theme switched to $NEW_FILE"
    ''
  );

  zigPkg = inputs.zig.packages.${pkgs.system}.master;
  zlsPkg = inputs.zls.packages.${pkgs.system}.default;
in
{
  home.packages = with pkgs; [
    theme-switcher

    libnotify
    gsettings-desktop-schemas
    gsettings-qt

    bc # Calculator
    bottom # System viewer
    ncdu # TUI disk usage
    eza # Better ls
    ripgrep # Better grep
    fd # Better find
    httpie # Better curl
    diffsitter # Better diff
    jq # JSON pretty printer and manipulator
    unzip
    unrar
    p7zip
    xdg-utils
    bat
    lazygit
    tree

    imhex
    hexyl
    difftastic
    bitwise

    # DEFAULT LANGUAGES i use mostly?
    zigPkg
    zlsPkg
    valgrind
    clang-tools
    llvmPackages_latest.libstdcxxClang
    llvmPackages_latest.libcxx
    llvmPackages_latest.lldb
    cppcheck
    gdb

    inputs.uwu-colors.packages.${pkgs.system}.default

    # language formatters
    # nodePackages.prettier
    # dprint
    # deno

    nixd # Nix LSP
    alejandra
    nixfmt-rfc-style # Nix formatter
    nvd # Differ
    nix-output-monitor
    nix-tree
    nh # Nice wrapper for NixOS and HM

    ltex-ls # Spell checking LSP
  ];

  home.persistence."/persist/home/${config.home.username}".directories = [
    ".local/cache/nix"
  ];

  programs.fzf = {
    enable = true;
    defaultOptions = [ "--color 16" ];
  };

  # enable dir-env integration
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
