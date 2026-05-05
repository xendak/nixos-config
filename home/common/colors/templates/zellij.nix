{ paletteSet, lib, ... }:
let
  m = paletteSet.palette;

  # Mapping for hex conversion
  hexToDecMap = {
    "0" = 0;
    "1" = 1;
    "2" = 2;
    "3" = 3;
    "4" = 4;
    "5" = 5;
    "6" = 6;
    "7" = 7;
    "8" = 8;
    "9" = 9;
    "a" = 10;
    "b" = 11;
    "c" = 12;
    "d" = 13;
    "e" = 14;
    "f" = 15;
    "A" = 10;
    "B" = 11;
    "C" = 12;
    "D" = 13;
    "E" = 14;
    "F" = 15;
  };

  # Helper to convert a 2-character hex pair to decimal
  hexPairToDec =
    s: (hexToDecMap.${builtins.substring 0 1 s} * 16) + (hexToDecMap.${builtins.substring 1 1 s});

  toRGB =
    hex:
    let
      c = lib.removePrefix "#" hex;
      r = lib.substring 0 2 c;
      g = lib.substring 2 2 c;
      b = lib.substring 4 2 c;
    in
    "${toString (hexPairToDec r)} ${toString (hexPairToDec g)} ${toString (hexPairToDec b)}";

  standardEmphasis = ''
    emphasis_0 ${toRGB m.red}
    emphasis_1 ${toRGB m.green}
    emphasis_2 ${toRGB m.blue}
    emphasis_3 ${toRGB m.magenta}
  '';
in
{
  "zellij/default.kdl" = ''
    themes {
        default {
            text_unselected {
                base ${toRGB m.fg}
                background ${toRGB m.bg}
                ${standardEmphasis}
            }
            text_selected {
                base ${toRGB m.fg}
                background ${toRGB m.surface_container_high}
                ${standardEmphasis}
            }
            ribbon_selected {
                base ${toRGB m.bg}
                background ${toRGB m.primary}
                emphasis_0 ${toRGB m.red}
                emphasis_1 ${toRGB m.orange}
                emphasis_2 ${toRGB m.magenta}
                emphasis_3 ${toRGB m.cyan}
            }
            ribbon_unselected {
                base ${toRGB m.bg}
                background ${toRGB m.white}
                emphasis_0 ${toRGB m.red}
                emphasis_1 ${toRGB m.fg}
                emphasis_2 ${toRGB m.cyan}
                emphasis_3 ${toRGB m.magenta}
            }
            table_title {
                base ${toRGB m.primary}
                background ${toRGB m.black}
                ${standardEmphasis}
            }
            table_cell_selected {
                base ${toRGB m.fg}
                background ${toRGB m.surface_container_high}
                ${standardEmphasis}
            }
            table_cell_unselected {
                base ${toRGB m.fg}
                background ${toRGB m.bg}
                ${standardEmphasis}
            }
            list_selected {
                base ${toRGB m.fg}
                background ${toRGB m.surface_container_high}
                ${standardEmphasis}
            }
            list_unselected {
                base ${toRGB m.fg}
                background ${toRGB m.bg}
                ${standardEmphasis}
            }
            frame_selected {
                base ${toRGB m.primary}
                background ${toRGB m.black}
                emphasis_0 ${toRGB m.red}
                emphasis_1 ${toRGB m.green}
                emphasis_2 ${toRGB m.magenta}
                emphasis_3 ${toRGB m.black}
            }
            frame_highlight {
                base ${toRGB m.orange}
                background ${toRGB m.black}
                emphasis_0 ${toRGB m.magenta}
                emphasis_1 ${toRGB m.orange}
                emphasis_2 ${toRGB m.orange}
                emphasis_3 ${toRGB m.orange}
            }
            exit_code_success {
                base ${toRGB m.green}
                background ${toRGB m.black}
                emphasis_0 ${toRGB m.cyan}
                emphasis_1 ${toRGB m.bg}
                emphasis_2 ${toRGB m.magenta}
                emphasis_3 ${toRGB m.blue}
            }
            exit_code_error {
                base ${toRGB m.red}
                background ${toRGB m.black}
                emphasis_0 ${toRGB m.yellow}
                emphasis_1 ${toRGB m.black}
                emphasis_2 ${toRGB m.black}
                emphasis_3 ${toRGB m.black}
            }
            multiplayer_user_colors {
                player_1 ${toRGB m.magenta}
                player_2 ${toRGB m.blue}
                player_3 ${toRGB m.black}
                player_4 ${toRGB m.yellow}
                player_5 ${toRGB m.green}
                player_6 ${toRGB m.black}
                player_7 ${toRGB m.red}
                player_8 ${toRGB m.black}
                player_9 ${toRGB m.black}
                player_10 ${toRGB m.black}
            }
        }
    }
    theme "default"
  '';
}
