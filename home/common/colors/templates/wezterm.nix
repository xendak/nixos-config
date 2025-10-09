{ paletteSet, ... }:
let
  p = paletteSet.palette;
in
{
  "wezterm/colors/current.lua" = # lua
    ''
      return {
        foreground = "${p.base05}",
        background = "${p.base00}",
        cursor_bg = "${p.cursor_bg}",
        cursor_fg = "${p.cursor_fg}",
        cursor_border = "${p.base05}",

        selection_fg = "${p.base00}",
        selection_bg = "${p.base05}",

        ansi = {
          "${p.base00}",
          "${p.base08}",
          "${p.base0B}",
          "${p.base0A}",
          "${p.base0D}",
          "${p.base0E}",
          "${p.base0C}",
          "${p.base05}",
        },

        brights = {
          "${p.base03}",
          "${p.base08}",
          "${p.base0B}",
          "${p.base0A}",
          "${p.base0D}",
          "${p.base0E}",
          "${p.base0C}",
          "${p.base07}",
        },

        tab_bar = {
          background = "${p.base01}",
          active_tab = {
            bg_color = "${p.base00}",
            fg_color = "${p.base05}",
          },
          inactive_tab = {
            bg_color = "${p.base01}",
            fg_color = "${p.base04}",
          },
        },
      }
    '';
}
