{
  config,
  ...
}:
{
  themes = {
    enable = true;
    activeTheme = "gorgoroth";
    palettesPath = ./palettes;
    templatesPath = ./templates;

    targets =
      let
        # theme = config.themes.activeTheme;
        home = config.home.homeDirectory;
      in
      {
        # GTK
        "gtk/gtk.css" = "${home}/.config/gtk-3.0/gtk.css";
        "gtk/gtk.css_clone_1" = "${home}/.config/gtk-3.0/gtk-dark.css";
        "gtk/settings.ini" = "${home}/.config/gtk-3.0/settings.ini";
        "gtk/gtk.css_clone_2" = "${home}/.config/gtk-4.0/gtk.css";
        "gtk/gtk.css_clone_3" = "${home}/.config/gtk-4.0/gtk-dark.css";
        "gtk/settings.ini_clone_1" = "${home}/.config/gtk-4.0/settings.ini";
        "gtk/settings.ini_clone_2" = "${home}/.config/gtk-2.0/gtkrc";

        # QT / KDE
        "qt/qt.conf" = "${home}/.config/qt5ct/qt5ct.conf";
        "qt/qt.conf_clone_1" = "${home}/.config/qt6ct/qt6ct.conf";
        "qt/colors.conf" = "${home}/.config/qt5ct/colors/current.conf";
        "qt/colors.conf_clone_1" = "${home}/.config/qt6ct/colors/current.conf";
        "qt/kdeglobals" = "${home}/.config/kdeglobals";

        # Fcitx5
        "fcitx5/theme.conf" = "${home}/.local/share/fcitx5/themes/current/theme.conf";
        "fcitx5/highlight.svg" = "${home}/.local/share/fcitx5/themes/current/highlight.svg";
        "fcitx5/panel.svg" = "${home}/.local/share/fcitx5/themes/current/panel.svg";

        # Zathura
        "zathura/zathurarc" = "${home}/.config/zathura/zathurarc";

        # Quickshell
        "quickshell/theme.txt" = "${home}/.local/state/caelestia/scheme/current.txt";

        # Vesktop / Discord
        "vesktop/quickCss.css" = "${home}/.config/vesktop/settings/quickCss.css";
        "vesktop/quickCss.css_clone_1" = "${home}/.config/discord/settings/quickCss.css";
        "vesktop/quickCss.css_clone_2" = "${home}/.config/discordcanary/settings/quickCss.css";

        # Helix
        "helix/themes/current.toml" = "${home}/.config/helix/themes/current.toml";

        # Wezterm
        "wezterm/colors/current.lua" = "${home}/.config/wezterm/colors/current.lua";

        # Foot
        "foot/colors.ini" = "${home}/.config/foot/colors.ini";

        # Emacs
        "emacs/themes/base16-nix-theme.el" = "${home}/.config/emacs/themes/base16-nix-theme.el";
        # "emacs/themes/current-theme.el" = "${home}/.config/emacs/themes/current-theme.el";
      };

  };
}
