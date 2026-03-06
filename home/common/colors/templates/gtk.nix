{ paletteSet, config, ... }:
let
  m = paletteSet.palette;
  settingsIni =
    if paletteSet.type == "dark" then
      # ini
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
      # ini
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
  "gtk/gtk.css" =
    # css
    ''
      /* ── Accent (primary) ────────────────────────────────────────────── */
      @define-color accent_color              ${m.primary};
      @define-color accent_bg_color           ${m.primary};
      @define-color accent_fg_color           ${m.on_primary};

      /* ── Destructive (error) ─────────────────────────────────────────── */
      @define-color destructive_color         ${m.error};
      @define-color destructive_bg_color      ${m.error};
      @define-color destructive_fg_color      ${m.on_error};

      /* ── Success (primary — M3 has no dedicated success token) ───────── */
      @define-color success_color             ${m.primary};
      @define-color success_bg_color          ${m.primary_container};
      @define-color success_fg_color          ${m.on_primary_container};

      /* ── Warning (tertiary) ──────────────────────────────────────────── */
      @define-color warning_color             ${m.tertiary};
      @define-color warning_bg_color          ${m.tertiary_container};
      @define-color warning_fg_color          ${m.on_tertiary_container};

      /* ── Error ───────────────────────────────────────────────────────── */
      @define-color error_color               ${m.error};
      @define-color error_bg_color            ${m.error_container};
      @define-color error_fg_color            ${m.on_error_container};

      /* ── Window ──────────────────────────────────────────────────────── */
      @define-color window_bg_color           ${m.bg};
      @define-color window_fg_color           ${m.on_background};

      /* ── View (content areas) ────────────────────────────────────────── */
      @define-color view_bg_color             ${m.surface};
      @define-color view_fg_color             ${m.on_surface};

      /* ── Headerbar ───────────────────────────────────────────────────── */
      @define-color headerbar_bg_color        ${m.surface_container};
      @define-color headerbar_fg_color        ${m.on_surface};
      @define-color headerbar_border_color    ${m.outline_variant};
      @define-color headerbar_backdrop_color  ${m.surface_container_low};
      @define-color headerbar_shade_color     ${m.surface_container_high};

      /* ── Cards / Dialogs ─────────────────────────────────────────────── */
      @define-color card_bg_color             ${m.surface_container};
      @define-color card_fg_color             ${m.on_surface};
      @define-color card_shade_color          rgba(0, 0, 0, 0.15);

      @define-color dialog_bg_color           ${m.surface_container_high};
      @define-color dialog_fg_color           ${m.on_surface};

      /* ── Popover ─────────────────────────────────────────────────────── */
      @define-color popover_bg_color          ${m.surface_container_low};
      @define-color popover_fg_color          ${m.on_surface};

      /* ── Sidebar ─────────────────────────────────────────────────────── */
      @define-color sidebar_bg_color          ${m.surface_container_low};
      @define-color secondary_sidebar_bg_color ${m.surface_container_lowest};
      @define-color sidebar_backdrop_color    ${m.surface_container_lowest};
      @define-color secondary_sidebar_backdrop_color ${m.surface_container_lowest};

      /* ── Misc ────────────────────────────────────────────────────────── */
      @define-color shade_color               rgba(0, 0, 0, 0.3);
      @define-color scrollbar_outline_color   ${m.outline_variant};

      .navigation-sidebar {
          background-color: @sidebar_bg_color;
          color: @window_fg_color;
      }

      headerbar.default-decoration {
          margin-bottom: 50px;
          margin-top: -100px;
      }

      /* rm -rf window shadows */
      window.csd,
      window.csd decoration {
          box-shadow: none;
      }
    '';

  "gtk/settings.ini" = settingsIni;
}
