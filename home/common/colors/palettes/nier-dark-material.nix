{ lib, ... }:
let
  lazy = false;
  must = {
    primary = "#ab4642";
    secondary = "#9b8d7f";
    tertiary = "#556677";
    error = "#bf616a";

    fg = "#bab5a1";
    bg = "#181818";
    surface_container = "#262626";
    surface_container_low = "#121212";
    surface_container_high = "#333333";

    dim = "#434343";
    cursor_bg = "#6b8e23";
    cursor_fg = "#171a20";
  };

  overrides = {
    comments = "#434343";
    keywords = "#746e61";
    labels = "#538080";
    punctuation = "#9b8d7f";
    macros = "#c0bcaa";
    functions = "#777777";
    strings = "#626B67";
    builtins = "#edbbb2";
    types = "#ebdbb2";
    specials = "#cecbbd";
    numeric = "#90A999";
    constants = "#8C7F70";
    modules = "#556677";
    tags = "#506070";

    surface_variant = "#bab5a1";
    on_surface_variant = "#282828";

    selection_fg = must.bg;
    selection_bg = must.primary;

    white = "#3c3836";
    red = "#cc241d";
    green = "#98971a";
    yellow = "#d79921";
    blue = "#458588";
    magenta = "#b16286";
    cyan = "#689d6a";
    gray = "#a89984";
    orange = "#d65d0e";
    black = "#ebdbb2";
  };

in
{
  isLazy = lazy;
  slug = "nier-dark-material";
  name = "nier-dark-material";
  type = "dark";
  author = "xendak";
  must = must;
  overrides = overrides;
}
