{ lib, ... }:
let
  lazy = false;
  must = {
    primary = "#6b8e23";
    secondary = "#ac7b0b";
    tertiary = "#478980";
    error = "#cc5735";

    fg = "#a08563";
    bg = "#0c0c0c";

    surface_container = "#181818";
    surface_container_low = "#101010";
    surface_container_high = "#262626";

    dim = "#686868";

    cursor_bg = "#00ee00";
    cursor_fg = "#0c0c0c";
  };

  overrides = {
    comments = "#686868";
    keywords = "#ac7b0b";
    labels = "#dab98f";
    punctuation = "#907553";
    macros = "#478980";
    functions = "#cc5735";
    strings = "#6b8e23";
    builtins = "#dab98f";
    types = "#d8a51d";
    specials = "#ab4642";
    numeric = "#6b8e23";
    constants = "#6b8e23";
    modules = "#dab98f";
    tags = "#ac7b0b";

    surface_variant = "#1f1f27";
    on_surface_variant = "#cb9401";

    selection_fg = "#c4b82b";
    selection_bg = "#315268";

    black = "#161616";
    red = "#ab4642";
    green = "#6b8e23";
    yellow = "#c47b11";
    blue = "#8BA4B0";
    magenta = "#A292A3";
    cyan = "#8EA4A2";
    white = "#C8C093";

    gray = "#7f7f7f";
    orange = "#d65d0e";
  };

in
{
  isLazy = lazy;
  slug = "handmadehero-material";
  name = "Handmadehero-material";
  type = "dark";
  author = "casey muratori";
  must = must;
  overrides = overrides;
}
