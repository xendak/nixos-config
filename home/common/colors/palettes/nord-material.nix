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
    black = "#3B4252";
    red = "#BF616A";
    green = "#A3BE8C";
    yellow = "#EBCB8B";
    blue = "#5E81AC";
    magenta = "#B48EAD";
    cyan = "#8FBCBB";
    white = "#E5E9F0";

    gray = "#4C566A";
    orange = "#D08770";

    brigthblack = "#4C566A";
    brightred = "#C5727A";
    brightgreen = "#B1C89D";
    brightyellow = "#EFD49F";
    brightblue = "#81A1C1";
    brightmagenta = "#BE9DB8";
    brightcyan = "#9FC6C5";
    brightwhite = "#E5E9F0";

    brightorange = "#D79784";
    brightgray = "#60728A";

    selection_fg = "#4A4458";
    outline = "#79747E";

    # REMINDER
    # Normal      Bright
    # --------    -------
    # 0 Black      8 Black
    # 1 Red        9 Red
    # 2 Yellow    10 Yellow
    # 3 Green     11 Green
    # 4 Blue      12 Blue
    # 5 Magenta   13 Magenta
    # 6 Cyan      14 Cyan
    # 7 White     15 White

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
