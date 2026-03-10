{ lib, ... }:
let
  lazy = false;
  must = {
    primary = "#ab4642";
    secondary = "#194820";
    tertiary = "#393e51";
    error = "#bf616a";

    fg = "#000000";
    bg = "#b1ab92";
    surface_container = "#c5bfa6";
    surface_container_low = "#948d71";
    surface_container_high = "#cbc5af";

    cursor_bg = "#ab4642";
    cursor_fg = "#101010";
  };

  overrides = {
    white = "#282828";
    grey = "#817b78";
    orange = "#CA397C";
    red = "#9c341f";
    green = "#795b00";
    yellow = "#663d52";
    blue = "#ab4642";
    magenta = "#ac6561";
    cyan = "#454320";

    selection_fg = "#202020";
  };

in
{
  isLazy = lazy;
  slug = "nier-material";
  name = "nier-material";
  type = "dark";
  author = "xendak";
  must = must;
  overrides = overrides;
}
