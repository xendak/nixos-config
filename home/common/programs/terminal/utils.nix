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
      QUICKSHELL_THEME_DIR="$HOME/Flake/home/common/programs/quickshell/themes"
      QUICKSHELL_CURRENT_THEME_DIR="$HOME/.local/state/caelestia/scheme"
      GTK_QT_THEME_DIR="$HOME/.config/themes"

      case $1 in
        "dark")
          NEW_FILE="dark"
          GTK_QT_FILE="dark"
          export GTK_THEME="${config.gtk.theme.name}-dark"
          gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
          dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
        ;;
        "light")
          NEW_FILE="light"
          GTK_QT_FILE="light"
          export GTK_THEME="${config.gtk.theme.name}"
          gsettings set org.gnome.desktop.interface color-scheme "prefer-light"
          dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'"
        ;;
        *)
          NEW_FILE="default"
          GTK_QT_FILE="dark"
          export GTK_THEME="${config.gtk.theme.name}-dark"
          dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
          gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
        ;;
      esac

      mkdir -p "$HOME/.config/gtk-2.0"
      cat "$GTK_QT_THEME_DIR/gtk-$GTK_QT_FILE.ini" >"$GTK_QT_THEME_DIR/gtkrc.ini"
      mkdir -p "$HOME/.config/gtk-3.0"
      cat "$GTK_QT_THEME_DIR/gtk-$GTK_QT_FILE.ini" >"$GTK_QT_THEME_DIR/settings.ini"
      mkdir -p "$HOME/.config/gtk-4.0"
      cat "$GTK_QT_THEME_DIR/gtk-$GTK_QT_FILE.ini" >"$GTK_QT_THEME_DIR/settings.ini"
      mkdir -p "$HOME/.config/qt5ct"
      cat "$GTK_QT_THEME_DIR/qt-$GTK_QT_FILE.conf" >"$GTK_QT_THEME_DIR/qt5ct.conf"
      mkdir -p "$HOME/.config/qt6ct"
      cat "$GTK_QT_THEME_DIR/qt-$GTK_QT_FILE.conf" >"$GTK_QT_THEME_DIR/qt6ct.conf"

      cp -r "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets" "$HOME/.config/gtk-4.0" 
      cp -r "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css" "$HOME/.config/gtk-4.0/gtk.css" 
      cp -r "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css" "$HOME/.config/gtk-4.0/gtk-dark.css" 


      cat "$HELIX_THEME_DIR/$NEW_FILE.toml" >"$HELIX_THEME_DIR/current.toml"
      sed -i "s/##/#/g" "$HELIX_THEME_DIR/current.toml"

      cat "$WEZTERM_THEME_DIR/$NEW_FILE.lua" >"$WEZTERM_THEME_DIR/current.lua"
      sed -i "s/##/#/g" "$WEZTERM_THEME_DIR/current.lua"

      mkdir -p $QUICKSHELL_CURRENT_THEME_DIR
      cat "$QUICKSHELL_THEME_DIR/$NEW_FILE.txt" >"$QUICKSHELL_CURRENT_THEME_DIR/current.txt"

      echo $(date +"%d/%m/%y | %H:%M >") "Theme switched to $NEW_FILE." >> /tmp/theme-switcher
      pkill -USR1 hx
      notify-send "Theme Manager" --expire-time=2000 --app-name="Theme Manager" --icon=preferences-desktop-theme "Theme switched to $NEW_FILE"
    ''
  );
in
{
  home.packages = with pkgs; [
    theme-switcher
    libnotify
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

  programs.fzf = {
    enable = true;
    defaultOptions = [ "--color 16" ];
  };

  # enable dir-env integration
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  home.file = {
    ".config/themes/.keep".text = "";

    # --- GTK Theme Files ---
    ".config/themes/gtk-dark.ini" = {
      # Here you could use a theme generator instead of hardcoding the text
      # text = pkgs.matugen.override { ... };
      text = ''
        [Settings]
        gtk-application-prefer-dark-theme=1
        gtk-theme-name="${config.gtk.theme.name}-dark"
        gtk-icon-theme-name="${config.gtk.iconTheme.name}-Dark"
        gtk-font-name=""${config.fontProfiles.regular.family} 12 10""
        gtk-cursor-theme-name="${config.gtk.cursorTheme.name}"
        gtk-cursor-theme-size=${toString config.gtk.cursorTheme.size}
      '';
    };

    ".config/themes/gtk-light.ini" = {
      text = ''
        [Settings]
        gtk-application-prefer-dark-theme=0
        gtk-theme-name="${config.gtk.theme.name}"
        gtk-icon-theme-name="${config.gtk.iconTheme.name}"
        gtk-font-name="${config.fontProfiles.regular.family} 12 10"
        gtk-cursor-theme-name="${config.gtk.cursorTheme.name}"
        gtk-cursor-theme-size=${toString config.gtk.cursorTheme.size}
      '';
    };

    # --- Qt5 Theme Files ---
    ".config/themes/qt-dark.conf" = {
      text = ''
        [Appearance]
        custom_palette=false
        icon_theme=${config.gtk.iconTheme.name}-Dark
        standard_dialogs=gtk3
        style=gtk3

        [Fonts]
        fixed="${config.fontProfiles.monospace.family},12,-1,5,50,0,0,0,0,0"
        general="${config.fontProfiles.regular.family},12,-1,5,50,0,0,0,0,0"

        [Interface]
        activate_item_on_single_click=1
        buttonbox_layout=0
        cursor_flash_time=1000
        dialog_buttons_have_icons=1
        double_click_interval=400
        gui_effects=@Invalid()
        keyboard_scheme=2
        menus_have_icons=true
        show_shortcuts_in_context_menus=true
        stylesheets=@Invalid()
        toolbutton_style=4
        underline_shortcut=1
        wheel_scroll_lines=3

        [SettingsWindow]
        geometry="@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=\0\0\0\0\0\0\0\0\0\0\x4\xff\0\0\x5w\0\0\0\0\x2\0\0\0\n\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=)"

        [Troubleshooting]
        force_raster_widgets=1
        ignored_applications=@Invalid()
      '';
    };

    ".config/themes/qt5light.conf" = {
      text = ''
        [Appearance]
        custom_palette=false
        icon_theme=${config.gtk.iconTheme.name}
        standard_dialogs=gtk3
        style=gtk3

        [Fonts]
        fixed="${config.fontProfiles.monospace.family},12,-1,5,50,0,0,0,0,0"
        general="${config.fontProfiles.regular.family},12,-1,5,50,0,0,0,0,0"

        [Interface]
        activate_item_on_single_click=1
        buttonbox_layout=0
        cursor_flash_time=1000
        dialog_buttons_have_icons=1
        double_click_interval=400
        gui_effects=@Invalid()
        keyboard_scheme=2
        menus_have_icons=true
        show_shortcuts_in_context_menus=true
        stylesheets=@Invalid()
        toolbutton_style=4
        underline_shortcut=1
        wheel_scroll_lines=3

        [SettingsWindow]
        geometry="@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=\0\0\0\0\0\0\0\0\0\0\x4\xff\0\0\x5w\0\0\0\0\x2\0\0\0\n\0\0\0\0\0\0\0\0\0\0\0\x4\xd3\0\0\x5=)"

        [Troubleshooting]
        force_raster_widgets=1
        ignored_applications=@Invalid()
      '';
    };
  };

}
