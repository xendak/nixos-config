{
  config,
  ...
}:
{
  themes = {
    enable = true;
    theme = "gorgoroth-material";
    palettesPath = ./palettes;
    templatesPath = ./templates;
    wallpaper = "/home/${config.home.username}/Flake/home/common/wallpapers/13.jpg";

    targets =
      let
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
        "qt/current.colors" = "${home}/.local/share/color-schemes/current.colors";

        # Browsers
        "zen/userChrome.css" = "${home}/.config/zen/${config.home.username}/chrome/userChrome.css";
        "zen/userContent.css" = "${home}/.config/zen/${config.home.username}/chrome/userContent.css";

        # Obsidian
        "obsidian/theme.css" = "${home}/Documents/Notes/xendak/.obsidian/snippets/material-theme.css";

        # Fcitx5
        "fcitx5/theme.conf" = "${home}/.local/share/fcitx5/themes/current/theme.conf";
        "fcitx5/highlight.svg" = "${home}/.local/share/fcitx5/themes/current/highlight.svg";
        "fcitx5/panel.svg" = "${home}/.local/share/fcitx5/themes/current/panel.svg";

        # Zathura
        "zathura/zathurarc" = "${home}/.config/zathura/zathurarc";

        # Quickshell ( half of it is done by itself )
        # "quickshell/theme.txt" = "${home}/.local/state/caelestia/scheme/current.txt";
        "quickshell/theme.txt" = "${home}/.local/state/caelestia/scheme/preview.txt";

        # Vesktop / Discord
        "vesktop/quickCss.css" = "${home}/.config/vesktop/settings/quickCss.css";
        "discord/settings.json" = "${home}/.config/discord/settings.json";
        "discord/settings.json_clone_1" = "${home}/.config/discordcanary/settings.json";

        # Helix
        "helix/themes/current.toml" = "${home}/.config/helix/themes/current.toml";

        # Nvim
        "nvim/colors.vim" = "${home}/.config/nvim/colors.vim";

        # Niri
        "niri/colors.kdl" = "${home}/.config/niri/colors.kdl";

        # Yazi
        "yazi/theme.toml" = "${home}/.config/yazi/theme.toml";

        # Lazygit
        "lazygit/config.yml" = "${home}/.config/lazygit/config.yml";

        # Wezterm
        "wezterm/colors/current.lua" = "${home}/.config/wezterm/colors/current.lua";

        # Foot
        "foot/colors.ini" = "${home}/.config/foot/colors.ini";

        # Emacs
        "emacs/themes/custom-nix-theme.el" = "${home}/.config/emacs/themes/custom-nix-theme.el";
      };

  };
}
