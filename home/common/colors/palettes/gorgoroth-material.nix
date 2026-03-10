{ lib, ... }:
let
  lazy = false;
  must = {
    primary = "#9b8d7f";
    secondary = "#888888";
    tertiary = "#aaaaaa";
    error = "#bf616a";

    fg = "#c1c1c1";
    bg = "#181818";
    surface_container = "#262626";
    surface_container_low = "#121212";
    surface_container_high = "#333333";

    cursor_bg = "#e3556f";
    cursor_fg = "#171a20";
  };

  overrides = {
    white = "#141414";
    grey = "#434343";
    red = "#5f8787";
    orange = "#8c7f70";
    green = "#9b8d7f";
    blue = "#888888";
    yellow = "#aaaaaa";
    magenta = "#999999";
    cyan = "#8fbcbb";
    selection_fg = "#4A4458";
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
