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
    brightwhite = "#191D24";
    white = "#1E222A";
    gray = "#4C566A";
    brightgray = "#60728A";
    black = "#E5E9F0";
    brightblack = "#ECEFF4";
    blue = "#5E81AC";
    brightblue = "#81A1C1";
    cyan = "#8FBCBB";
    brightcyan = "#9FC6C5";
    red = "#BF616A";
    brightred = "#C5727A";
    orange = "#D08770";
    brightorange = "#D79784";
    yellow = "#EBCB8B";
    brightyellow = "#EFD49F";
    green = "#A3BE8C";
    brightgreen = "#B1C89D";
    magenta = "#B48EAD";
    brightmagenta = "#BE9DB8";

    selection_fg = "#4A4458";
    outline = "#79747E";

    # REMINDER
    # keywords = extra.keywords or ansi.red;
    # labels = extra.labels or ansi.brightred;
    # strings = extra.strings or ansi.green;
    # alt_functions = extra.alt_functions or ansi.brightgreen;
    # builtins = extra.builtins or ansi.yellow;
    # types = extra.types or ansi.brightyellow;
    # functions = extra.functions or ansi.blue;
    # macros = extra.macros or ansi.brightblue;
    # specials = extra.specials or ansi.magenta;
    # constants = extra.constants or ansi.brightmagenta;
    # modules = extra.modules or ansi.cyan;
    # tags = extra.tags or ansi.brightcyan;
    # numeric = extra.numeric or ansi.orange;
    # punctuation = extra.punctuation or ansi.brightorange;
    # comments = extra.comments or ansi.gray;
    # inlay = extra.inlay or ansi.brightgray;
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
