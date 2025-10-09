{ paletteSet, config, ... }:
let
  p = paletteSet.palette;
  settingsIni =
    if paletteSet.type == "dark" then
      ''
        [Settings]
        gtk-application-prefer-dark-theme=1
        gtk-theme-name="${config.gtk.theme.name}-dark"
        gtk-icon-theme-name="${config.gtk.iconTheme.name}-Dark"
        gtk-font-name="${config.fontProfiles.regular.family} 12 10"
        gtk-cursor-theme-name="${config.gtk.cursorTheme.name}"
        gtk-cursor-theme-size=${toString config.gtk.cursorTheme.size}
      ''
    else
      ''
        [Settings]
        gtk-application-prefer-dark-theme=0
        gtk-theme-name="${config.gtk.theme.name}"
        gtk-icon-theme-name="${config.gtk.iconTheme.name}"
        gtk-font-name="${config.fontProfiles.regular.family} 12 10"
        gtk-cursor-theme-name="${config.gtk.cursorTheme.name}"
        gtk-cursor-theme-size=${toString config.gtk.cursorTheme.size}
      '';
in
{
  "gtk/gtk.css" = ''
    @define-color accent_color ${p.base0D};
    @define-color accent_bg_color ${p.base0D};
    @define-color accent_fg_color ${p.base00};

    @define-color destructive_color ${p.base08};
    @define-color destructive_bg_color ${p.base08};
    @define-color destructive_fg_color ${p.base07};

    @define-color success_color ${p.base0B};
    @define-color success_bg_color ${p.base0B};
    @define-color success_fg_color ${p.base00};

    @define-color warning_color ${p.base0A};
    @define-color warning_bg_color ${p.base0A};
    @define-color warning_fg_color ${p.base00};

    @define-color error_color ${p.base08};
    @define-color error_bg_color ${p.base08};
    @define-color error_fg_color ${p.base07};

    @define-color window_bg_color ${p.base00};
    @define-color window_fg_color ${p.base05};

    @define-color view_bg_color ${p.base00};
    @define-color view_fg_color ${p.base05};

    @define-color headerbar_bg_color @window_bg_color;
    @define-color headerbar_fg_color @window_fg_color;
    @define-color headerbar_border_color @window_bg_color;
    @define-color headerbar_backdrop_color @window_bg_color;
    @define-color headerbar_shade_color @window_bg_color;

    @define-color card_bg_color ${p.base02};
    @define-color card_fg_color @window_fg_color;
    @define-color card_shade_color rgba(0, 0, 0, 0.2);

    @define-color dialog_bg_color @card_bg_color;
    @define-color dialog_fg_color @card_fg_color;

    @define-color popover_bg_color ${p.base01};
    @define-color popover_fg_color @window_fg_color;

    @define-color shade_color rgba(0, 0, 0, 0.36);
    @define-color scrollbar_outline_color rgba(0, 0, 0, 0.5);

    @define-color sidebar_bg_color ${p.base01};
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

  "gtk/settings.ini" = settingsIni;
}
