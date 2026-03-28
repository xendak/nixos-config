{ lib, ... }:
let
  lazy = false;
  must = {
    primary = "#5f8787";
    secondary = "#9b8d7f";
    tertiary = "#556677";
    error = "#bf616a";

    fg = "#c1c1c1";
    bg = "#181818";
    surface_container = "#262626";
    surface_container_low = "#121212";
    surface_container_high = "#333333";

    dim = "#434343";

    cursor_bg = "#e3556f";
    cursor_fg = "#171a20";
  };

  overrides = {
    comments = "#434343";
    keywords = "#5f8787";
    labels = "#538080";
    punctuation = "#9b8d7f";
    macros = "#888888";
    functions = "#777777";
    strings = "#626B67";
    builtins = "#aaaaaa";
    types = "#a0a0a0";
    specials = "#999999";
    numeric = "#90A999";
    constants = "#8C7F70";
    modules = "#556677";
    tags = "#506070";

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

    # white = "#3c3836";
    # red = "#ab4642";
    # green = "#98971a";
    # yellow = "#AF7C54";
    # blue = "#556677";
    # magenta = "#b16286";
    # cyan = "#689d6a";
    # gray = "#a89984";
    # orange = "#d65d0e";
    # black = "#ebdbb2";

    # brightwhite = "#504945";
    # brightred = "#fb4934";
    # brightgreen = "#b8bb26";
    # brightyellow = "#fabd2f";
    # brightblue = "#83a598";
    # brightmagenta = "#d3869b";
    # brightcyan = "#8ec07c";
    # brightgray = "#928374";
    # brightorange = "#fe8019";
    # brightblack = "#fbf1c7";

  };

in
{
  isLazy = lazy;
  slug = "gorgoroth-material";
  name = "gorgoroth-material";
  type = "dark";
  author = "xendak";
  must = must;
  overrides = overrides;
}
