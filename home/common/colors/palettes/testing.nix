{ lib, ... }:
let
  lazy = true;
  must = {
    primary = "#8FCFF3";
    secondary = "#B5C9D7";
    tertiary = "#C9C1EA";
    error = "#FFB4AB";

    fg = "#DFE3E7";
    bg = "#202020";
    surface_container = "#121212";
    surface_container_high = "#303030";
    dim = "#434343";

    cursor_bg = "#e3556f";
    cursor_fg = "#101010";
  };

  overrides = {
    red = "#bf616a";
    orange = "#d08770";
    yellow = "#ebcb8b";
    green = "#a3be8c";
    magenta = "#b48ead";
    blue = "#81a1c1";
    white = "#eceff4";
    # optional ?
    # selection_bg = ;
    # selection_fg = ;
  };

in
{
  isLazy = lazy;
  slug = "testing";
  name = "testing";
  type = "dark";
  author = "xendak";
  must = must;
  overrides = overrides;
}
