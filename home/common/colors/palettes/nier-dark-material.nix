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
    strings = "#8a9a7b";
    builtins = "#edbbb2";
    types = "#ebdbb2";
    specials = "#cecbbd";
    numeric = "#c4746e";
    constants = "#8C7F70";
    modules = "#556677";
    tags = "#506070";

    surface_variant = "#bab5a1";
    on_surface_variant = "#282828";

    selection_fg = must.bg;
    selection_bg = must.primary;

    black = "#353535";
    red = "#C4746E";
    green = "#8A9A7B";
    yellow = "#C4B28A";
    blue = "#8BA4B0";
    magenta = "#A292A3";
    cyan = "#8EA4A2";
    white = "#C8C093";

    gray = "#a89984";
    orange = "#d65d0e";

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
