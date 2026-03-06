{ paletteSet, ... }:
let
  p = paletteSet.palette;
in
{
  #   0  bg        8  outline
  #   1  error     9  error
  #   2  primary   10 primary
  #   3  tertiary  11 tertiary
  #   4  secondary 12 secondary
  #   5  on_sec_c  13 on_sec_c
  #   6  on_pri_c  14 on_pri_c
  #   7  on_surf   15 inverse_on_surface
  "wezterm/colors/current.lua" = # lua
    ''
      return {
        foreground    = "${p.on_surface}",
        background    = "${p.background}",
        cursor_bg     = "${p.cursor_bg or p.primary}",
        cursor_fg     = "${p.cursor_fg or p.on_primary}",
        cursor_border = "${p.primary}",

        selection_fg  = "${p.on_primary_container}",
        selection_bg  = "${p.primary_container}",

        ansi = {
          "${p.background}",
          "${p.error}",
          "${p.primary}",
          "${p.tertiary}",
          "${p.secondary}",
          "${p.on_secondary_container}",
          "${p.on_primary_container}",
          "${p.on_surface}",
        },

        brights = {
          "${p.outline}",
          "${p.error}",
          "${p.primary}",
          "${p.tertiary}",
          "${p.secondary}",
          "${p.on_secondary_container}",
          "${p.on_primary_container}",
          "${p.inverse_on_surface}",
        },

        tab_bar = {
          background = "${p.surface_container_low}",
          active_tab = {
            bg_color = "${p.primary_container}",
            fg_color = "${p.on_primary_container}",
          },
          inactive_tab = {
            bg_color = "${p.surface_container_low}",
            fg_color = "${p.outline}",
          },
          inactive_tab_hover = {
            bg_color = "${p.surface_container}",
            fg_color = "${p.on_surface_variant}",
          },
          new_tab = {
            bg_color = "${p.surface_container_low}",
            fg_color = "${p.outline}",
          },
        },
      }
    '';
}
