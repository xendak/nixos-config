{ lib, ... }:
let
  lazy = false;
  must = {
    primary = "#ab4642";
    secondary = "#578d4e";
    tertiary = "#454138";
    error = "#ab4642";

    fg = "#101010";
    bg = "#bab5a1";
    surface_container = "#c5bfa6";
    surface_container_low = "#948d71";
    surface_container_high = "#cbc5af";

    dim = "#6b656a";
    cursor_bg = "#ab4642";
    cursor_fg = "#101010";
  };

  overrides = {
    # default colors for ansi on white
    black = "#948d71";
    gray = "#746e61";
    white = "#3c3836";
    red = "#9d0006";
    green = "#225d18";
    yellow = "#854600";
    blue = "#076678";
    magenta = "#6a2f53";
    cyan = "#305a4b";
    orange = "#af3a03";

    brightblack = "#948d71";
    brightgray = "#746e61";
    brightwhite = "#3c3836";
    brightred = "#9d0006";
    brightgreen = "#225d18";
    brightyellow = "#854600";
    brightblue = "#076678";
    brightmagenta = "#6a2f53";
    brightcyan = "#305a4b";
    brightorange = "#af3a03";

    # brightwhite = "#928374";
    # brightblack = "#7c6f64";

    strings = overrides.red;
    numeric = "#225d18";

    comments = "#6b656a";
    keywords = "#252525";
    labels = "#3a3a3a";
    punctuation = "#555555";
    macros = "#333333";
    functions = "#1e1e1e";
    builtins = "#4a4a4a";
    types = "#2e2e2e";
    specials = "#444444";
    constants = "#3d3d3d";
    modules = "#2a2a2a";
    tags = "#353535";

    selection_fg = "#202020";
    selection_bg = "#DEB887";
  };

in
{
  isLazy = lazy;
  slug = "nier-material";
  name = "nier-material";
  # TODO: zen browser disregards this shit apparently ??
  type = "dark"; # maybe this is bad ? most of programs seem to disable the light zz
  author = "xendak";
  must = must;
  overrides = overrides;
}
