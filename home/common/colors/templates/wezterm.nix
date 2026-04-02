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

        selection_fg  = "${p.selection_fg}",
        selection_bg  = "${p.selection_bg}",

        copy_mode_active_highlight_bg = { Color = '#000000' },
        copy_mode_active_highlight_fg = { Color = 'Black' },
        copy_mode_inactive_highlight_bg = { Color = '#52ad70' },
        copy_mode_inactive_highlight_fg = { Color = 'White' },

        quick_select_label_bg = { Color = '${p.cursor_bg}' },
        quick_select_label_fg = { Color = '${p.cursor_fg}' },
        quick_select_match_bg = { Color = '${p.cursor_bg}' },
        quick_select_match_fg = { Color = '${p.cursor_fg}' },

        input_selector_label_bg = { Color = '${p.cursor_bg}' },
        input_selector_label_fg = { Color = '${p.cursor_fg}' },

        launcher_label_bg = { Color = '${p.cursor_bg}' },
        launcher_label_fg = { Color = '${p.cursor_fg}' },

        ansi = {
          "${p.black}",
          "${p.red}",
          "${p.green}",
          "${p.yellow}",
          "${p.blue}",
          "${p.magenta}",
          "${p.cyan}",
          "${p.white}",
        },

        brights = {
          "${p.brightblack}",
          "${p.brightred}",
          "${p.brightgreen}",
          "${p.brightyellow}",
          "${p.brightblue}",
          "${p.brightmagenta}",
          "${p.brightcyan}",
          "${p.brightwhite}",
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
