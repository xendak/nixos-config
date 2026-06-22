{ lib, ... }:
let
  lazy = false;
  must = {
    primary = "#8FCFF3";
    secondary = "#5e81ac";
    tertiary = "#8fbcbb";
    error = "#bf616a";

    fg = "#e5e9f0";
    bg = "#20242c";
    surface_container = "#2e3440";
    surface_container_low = "#171a20";
    surface_container_high = "#171a20";

    dim = "#4C566A";

    cursor_bg = "#e3556f";
    cursor_fg = "#171a20";

  };

  overrides = {
    primary = "#E06C75";
    on_primary = "#3B090E";
    primary_container = "#7C2838";
    on_primary_container = "#FFDAD9";
    inverse_primary = "#90283A";
    primary_fixed = "#FFDAD9";
    primary_fixed_dim = "#E0B3B7";
    on_primary_fixed = "#3B090E";
    on_primary_fixed_variant = "#7C2838";

    secondary = "#B6C4D8";
    on_secondary = "#0C1422";
    secondary_container = "#2D3A4E";
    on_secondary_container = "#D1DFF0";
    secondary_fixed = "#D1DFF0";
    secondary_fixed_dim = "#B6C4D8";
    on_secondary_fixed = "#0C1422";
    on_secondary_fixed_variant = "#2D3A4E";

    tertiary = "#5B9BF0";
    on_tertiary = "#041426";
    tertiary_container = "#163050";
    on_tertiary_container = "#D0E2FF";
    tertiary_fixed = "#D0E2FF";
    tertiary_fixed_dim = "#A4C7FF";
    on_tertiary_fixed = "#001C3A";
    on_tertiary_fixed_variant = "#163050";

    error = "#FF8A80";
    on_error = "#5C1612";
    error_container = "#7C2D1F";
    on_error_container = "#FFDAD6";

    background = "#090E1A";
    foreground = "#E1E7EF";
    bg = "#090E1A";
    fg = "#E1E7EF";

    on_background = "#E1E7EF";
    surface = "#070A12";
    on_surface = "#E1E7EF";
    surface_variant = "#1C2536";
    on_surface_variant = "#8899AF";

    surface_dim = "#060911";
    surface_bright = "#38476B";
    surface_container_lowest = "#030509";
    surface_container_low = "#0A0F1A";
    surface_container = "#121A2B";
    surface_container_high = "#1D2942";
    surface_container_highest = "#2B3B5E";
    dim = "#1C2536";
    outline = "#384658";
    outline_variant = "#1C2536";
    inverse_surface = "#E1E7EF";
    inverse_on_surface = "#1A2332";

    surface_tint = "#E06C75";
    shadow = "#000000";
    scrim = "#000000";
    source_color = "#E06C75";

    black = "#3B4252";
    red = "#BF616A";
    green = "#A3BE8C";
    yellow = "#EBCB8B";
    blue = "#5E81AC";
    magenta = "#B48EAD";
    cyan = "#8FBCBB";
    white = "#E5E9F0";
    gray = "#4C566A";
    orange = "#D08770";

    brightblack = "#4C566A";
    brightred = "#C5727A";
    brightgreen = "#B1C89D";
    brightyellow = "#EFD49F";
    brightblue = "#81A1C1";
    brightmagenta = "#BE9DB8";
    brightcyan = "#9FC6C5";
    brightwhite = "#E5E9F0";
    brightorange = "#D79784";
    brightgray = "#60728A";
  };

in
{
  isLazy = lazy;
  slug = "odysseus";
  name = "odysseus";
  type = "dark";
  author = "xendak";
  must = must;
  overrides = overrides;
}
