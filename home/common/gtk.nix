{
  config,
  pkgs,
  lib,
  ...
}:
# let
# inherit
# (inputs.nix-colors.lib-contrib {inherit pkgs;})
# gtkThemeFromScheme
# ;
# in
let
  # 1. Define your cursor theme details in one place
  cursorTheme = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 32;
  };

  # 2. Create the .reg file declaratively
  wineCursorReg = pkgs.writeText "wine-cursor-theme.reg" ''
    Windows Registry Editor Version 5.00

    [HKEY_CURRENT_USER\Control Panel\Cursors]
    "Arrow"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/left_ptr"
    "AppStarting"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/watch"
    "Hand"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/hand2"
    "Help"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/question_arrow"
    "No"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/crossed_circle"
    "NWPen"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/pencil"
    "SizeAll"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/sizing"
    "SizeNESW"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/nesw-resize"
    "SizeNS"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/ns-resize"
    "SizeNWSE"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/nwse-resize"
    "SizeWE"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/we-resize"
    "UpArrow"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/up_arrow"
    "Wait"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/watch"
    "Crosshair"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/cross"
    "IBeam"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/xterm"
    "Link"="${cursorTheme.package}/share/icons/${cursorTheme.name}/cursors/hand2"
  '';

  # THEME_SWITCHER
  defaultPalette = config.themes.default.colorScheme.palette;
  darkPalette = config.themes.dark.colorScheme.palette;
  lightPalette = config.themes.light.colorScheme.palette;

  mkGtkColorScheme =
    palette: with palette; ''
      @define-color accent_color ${base0D};
      @define-color accent_bg_color ${base0D};
      @define-color accent_fg_color ${base00};

      @define-color destructive_color ${base08};
      @define-color destructive_bg_color ${base08};
      @define-color destructive_fg_color ${base07};

      @define-color success_color ${base0B};
      @define-color success_bg_color ${base0B};
      @define-color success_fg_color ${base00};

      @define-color warning_color ${base0A};
      @define-color warning_bg_color ${base0A};
      @define-color warning_fg_color ${base00};

      @define-color error_color ${base08};
      @define-color error_bg_color ${base08};
      @define-color error_fg_color ${base07};

      @define-color window_bg_color ${base00};
      @define-color window_fg_color ${base05};

      @define-color view_bg_color ${base00};
      @define-color view_fg_color ${base05};

      @define-color headerbar_bg_color @window_bg_color;
      @define-color headerbar_fg_color @window_fg_color;
      @define-color headerbar_border_color @window_bg_color;
      @define-color headerbar_backdrop_color @window_bg_color;
      @define-color headerbar_shade_color @window_bg_color;

      @define-color card_bg_color ${base02};
      @define-color card_fg_color @window_fg_color;
      @define-color card_shade_color rgba(0, 0, 0, 0.2);

      @define-color dialog_bg_color @card_bg_color;
      @define-color dialog_fg_color @card_fg_color;

      @define-color popover_bg_color ${base01};
      @define-color popover_fg_color @window_fg_color;

      @define-color shade_color rgba(0, 0, 0, 0.36);
      @define-color scrollbar_outline_color rgba(0, 0, 0, 0.5);

      @define-color sidebar_bg_color ${base01};
      @define-color secondary_sidebar_bg_color @sidebar_bg_color;
      @define-color sidebar_backdrop_color @sidebar_bg_color;
      @define-color secondary_sidebar_backdrop_color @sidebar_bg_color;

      .navigation-sidebar {
          background-color: @sidebar_bg_color;
          color: @window_fg_color;
      }

      headerbar.default-decoration {
          margin-bottom: 50px;
          margin-top: -100px;
      }

      /* rm -rf window shadows */
      window.csd,              /* gtk4? */
      window.csd decoration { /* gtk3 */
          box-shadow: none;
      }
    '';
  defaultScheme = mkGtkColorScheme defaultPalette;
  darkScheme = mkGtkColorScheme darkPalette;
  lightScheme = mkGtkColorScheme lightPalette;

in
{
  home.pointerCursor = {
    # package = pkgs.bibata-cursors;
    # name = "Bibata-Modern-Classic";
    # size = 32;
    inherit (cursorTheme) package name size;
    gtk.enable = true;
    x11.enable = true;
  };

  # home.sessionVariables = { GTK_USE_PORTAL = "1"; };
  home.sessionVariables = {
    GTK2_RC_FILES = lib.mkDefault "${config.xdg.configHome}/gtk-2.0/gtkrc";
    XCURSOR_SIZE = lib.mkForce (builtins.toString cursorTheme.size);
    XCURSOR_THEME = lib.mkDefault cursorTheme.name;
    XCURSOR_PATH = lib.mkDefault "${config.gtk.cursorTheme.package}/share/icons/:$XCURSOR_PATH";
  };

  home.packages = [
    (pkgs.writeShellScriptBin "writeWriteCursorConfig" ''
      if [ "$(${pkgs.wine}/bin/wine reg query 'HKEY_CURRENT_USER\Control Panel\Cursors' /v Arrow 2> /dev/null | grep -c '${cursorTheme.name}')" -eq 0 ]; then
        echo "Applying Wine cursor theme..."
        ${pkgs.wine}/bin/regedit ${wineCursorReg}
      fi
    '')

    pkgs.gtk_engines
    pkgs.gtk4
    pkgs.adw-gtk3
    pkgs.catppuccin-gtk
    pkgs.adwaita-qt6
    pkgs.adwaita-qt
    pkgs.kdePackages.breeze
    pkgs.kdePackages.breeze-gtk
    # pkgs.kdePackages.breeze-icons
    # pkgs.libsForQt5.breeze-icons
    # pkgs.libsForQt5.breeze-qt5
    # pkgs.libsForQt5.breeze-gtk
    pkgs.gradience
    pkgs.papirus-icon-theme
    pkgs.papirus-folders
  ];

  gtk = {
    enable = true;
    font.name = "Sofia Pro 12";

    theme = {
      name = "adw-gtk3";
      package = pkgs.adw-gtk3;
    };
    # theme = {
    #   name = "${config.colorScheme.slug}";
    #   package = gtkThemeFromScheme {scheme = config.colorScheme;};
    # };
    #
    # iconTheme = {
    #   name = "Gruvbox-Plus-Dark";
    #   package = pkgs.gruvbox-plus-icons;
    # };

    # iconTheme = {
    #   name = "Colloid-Dark";
    #   package = pkgs.colloid-icon-theme;
    # };

    #      theme = {
    #        name = "Graphite-Dark-nord";
    #        package = (pkgs.graphite-gtk-theme.override { tweaks = ["nord"]; });
    #      };

    # theme = {
    #   name = "Orchis-Green-Dark-Compact";
    #   package = (pkgs.orchis-theme.override {
    #     themeVariants = [ "green" ];
    #     colorVariants = [ "dark" ];
    #     sizeVariants = [ "compact" ];
    #     tweaks = [ "compact" ];
    #     border-radius = 6;
    #   });
    # };

    # iconTheme = {
    #   name = "Tela circle dark";
    #   package = pkgs.tela-circle-icon-theme.override {
    #     colorVariants = ["nord"];
    #     circularFolder = true;
    #   };
    # };
    # iconTheme = {
    #   name = "WhiteSur";
    #   package = (
    #     pkgs.whitesur-icon-theme.override {
    #       boldPanelIcons = true;
    #       themeVariants = [ "green" ];
    #       alternativeIcons = true;
    #     }
    #   );
    # };
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };

    # iconTheme = {
    #   name = "Win10SurDark";
    #   package = (pkgs.win10sur.override {
    #     circleFolders = true;
    #     whitePanel = true;
    #   });
    # };

    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    # gtk3.extraConfig = {
    #   gtk-application-prefer-dark-theme = 1;
    # };
    # gtk4.extraConfig = {
    #   gtk-application-prefer-dark-theme = 1;
    # };
  };

  # Set GTK4 properly
  # home.file = {
  #   "${config.xdg.configHome}/gtk-4.0/gtk-dark.css" = {
  #     enable = true;
  #     source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
  #   };
  #   "${config.xdg.configHome}/gtk-4.0/gtk.css" = {
  #     enable = true;
  #     source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
  #   };
  #   "${config.xdg.configHome}/gtk-4.0/assets" = {
  #     enable = true;
  #     source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
  #   };
  #   ".local/share/icons/default/index.theme".text = ''
  #     [Icon Theme]
  #     Inherits=${config.gtk.cursorTheme.name}
  #   '';
  # };

  # theme-switcher configs
  home.file = {
    ".config/themes/.keep".text = "";

    # Matugen
    ".config/matugen/templates/matugen-gtk".text = ''
      @define-color accent_color {{colors.primary.default.hex}};
      @define-color accent_bg_color {{colors.primary_container.default.hex}};
      @define-color accent_fg_color {{colors.on_primary_container.default.hex}};
      @define-color destructive_color {{colors.error.default.hex}};
      @define-color destructive_bg_color {{colors.error_container.default.hex}};
      @define-color destructive_fg_color {{colors.on_error_container.default.hex}};
      @define-color success_color {{colors.secondary.default.hex}};
      @define-color success_bg_color {{colors.secondary_container.default.hex}};
      @define-color success_fg_color {{colors.on_secondary_container.default.hex}};
      @define-color warning_color {{colors.tertiary.default.hex}};
      @define-color warning_bg_color {{colors.tertiary_container.default.hex}};
      @define-color warning_fg_color {{colors.on_tertiary_container.default.hex}};
      @define-color error_color {{colors.error.default.hex}};
      @define-color error_bg_color {{colors.error_container.default.hex}};
      @define-color error_fg_color {{colors.on_error_container.default.hex}};
      @define-color window_bg_color {{colors.surface.default.hex}};
      @define-color window_fg_color {{colors.on_surface.default.hex}};
      @define-color view_bg_color {{colors.surface.default.hex}};
      @define-color view_fg_color {{colors.on_surface.default.hex}};
      @define-color headerbar_bg_color @window_bg_color;
      @define-color headerbar_fg_color @window_fg_color;
      @define-color headerbar_border_color @window_bg_color;
      @define-color headerbar_backdrop_color @window_bg_color;
      @define-color headerbar_shade_color @window_bg_color;
      @define-color card_bg_color {{colors.surface_container_high.default.hex}};
      @define-color card_fg_color {{colors.on_surface.default.hex}};
      @define-color card_shade_color rgba(0, 0, 0, 0.07);
      @define-color dialog_bg_color @card_bg_color;
      @define-color dialog_fg_color @card_fg_color;
      @define-color popover_bg_color @card_bg_color;
      @define-color popover_fg_color @card_fg_color;
      @define-color shade_color rgba(0, 0, 0, 0.36);
      @define-color scrollbar_outline_color rgba(139, 145, 152, 0.5);
      @define-color sidebar_bg_color {{colors.surface_container_low.default.hex}};
      @define-color secondary_sidebar_bg_color @sidebar_bg_color;
      @define-color sidebar_backdrop_color @sidebar_bg_color;
      @define-color secondary_sidebar_backdrop_color @sidebar_bg_color;

      .navigation-sidebar {
      	background-color: @sidebar_bg_color;
      	color: @window_fg_color;
      }


      headerbar.default-decoration {
        /* You may need to tweak these values depending on your GTK theme */
        margin-bottom: 50px;
        margin-top: -100px;
      }

      /* rm -rf window shadows */
      window.csd,             /* gtk4? */
      window.csd decoration { /* gtk3 */
        box-shadow: none;
      }
    '';
    ".config/matugen/config.toml".text = ''
      [config]
      [templates.gtk3]
      input_path = '~/.config/matugen/templates/matugen-gtk.css'
      output_path = '~/.config/gtk-3.0/gtk.css'

      [templates.gtk4]
      input_path = '~/.config/matugen/templates/matugen-gtk.css'
      output_path = '~/.config/gtk-4.0/gtk.css'
    '';

    ".config/themes/gtk-default.css".text = defaultScheme;
    ".config/themes/gtk-dark.css".text = darkScheme;
    ".config/themes/gtk-light.css".text = lightScheme;

    ".config/themes/gtk-dark.ini" = {
      text = ''
        [Settings]
        gtk-application-prefer-dark-theme=1
        gtk-theme-name="${config.gtk.theme.name}-dark"
        gtk-icon-theme-name="${config.gtk.iconTheme.name}-Dark"
        gtk-font-name="${config.fontProfiles.regular.family} 12 10"
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

  };

  # prefer dark
  # dconf.settings = {
  #   "org/gnome/desktop/interface" = {
  #     color-scheme = "prefer-dark";
  #   };
  # };
}
