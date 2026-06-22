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

    secondary = "#A9D79F";
    on_secondary = "#0C210E";
    secondary_container = "#334F32";
    on_secondary_container = "#C6EFBD";
    secondary_fixed = "#C6EFBD";
    secondary_fixed_dim = "#A9D79F";
    on_secondary_fixed = "#0C210E";
    on_secondary_fixed_variant = "#334F32";

    tertiary = "#B6C4D8";
    on_tertiary = "#0C1422";
    tertiary_container = "#2D3A4E";
    on_tertiary_container = "#D1DFF0";
    tertiary_fixed = "#D1DFF0";
    tertiary_fixed_dim = "#B6C4D8";
    on_tertiary_fixed = "#0C1422";
    on_tertiary_fixed_variant = "#2D3A4E";

    error = "#E46876";
    on_error = "#601410";
    error_container = "#674847";
    on_error_container = "#FFDAD6";

    background = "#1E1F28";
    foreground = "#E6E1E5";
    bg = "#1E1F28";
    fg = "#E6E1E5";

    on_background = "#E6E1E5";
    surface = "#0C0D11";
    on_surface = "#E6E1E5";
    surface_variant = "#ACA7B6";
    on_surface_variant = "#C9C4D0";

    surface_dim = "#12131A";
    surface_bright = "#454756";
    surface_container_lowest = "#060709";
    surface_container_low = "#12131A";
    surface_container = "#1E1F28";
    surface_container_high = "#2D2E3B";
    surface_container_highest = "#3F4152";

    dim = "#938F99";
    outline = "#ACA7B6";
    outline_variant = "#938F99";
    inverse_surface = "#E6E1E5";
    inverse_on_surface = "#1E1F28";

    surface_tint = "#D0BCFF";
    shadow = "#000000";
    scrim = "#000000";
    source_color = "#6750A4";

    black = "#353535";
    red = "#C4746E";
    green = "#8A9A7B";
    yellow = "#C4B28A";
    blue = "#8BA4B0";
    magenta = "#A292A3";
    cyan = "#8EA4A2";
    white = "#C8C093";
    gray = "#a89984";
    orange = "#D08770";

    brightblack = "#454545";
    brightred = "#E46876";
    brightgreen = "#87A987";
    brightyellow = "#E6C384";
    brightblue = "#7FB4CA";
    brightmagenta = "#938AA9";
    brightcyan = "#7AA89F";
    brightwhite = "#A6A69C";
    brightgray = "#928374";
    brightorange = "#D79784";
  };

in
{
  isLazy = lazy;
  slug = "archdaemon";
  name = "archdaemon";
  type = "dark";
  author = "xendak";
  must = must;
  overrides = overrides;
}
