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

    cursor_bg = "#e3556f";
    cursor_fg = "#171a20";

  };

  overrides = {
    red = "#bf616a";
    orange = "#d08770";
    yellow = "#ebcb8b";
    green = "#a3be8c";
    magenta = "#b48ead";
    blue = "#81a1c1";
    cyan = "#8fbcbb";
    white = "#eceff4";
    selection_fg = "#4A4458";
  };

in
{
  isLazy = lazy;
  slug = "nord-material";
  name = "nord-material";
  type = "dark";
  author = "xendak";
  must = must;
  overrides = overrides;
}
