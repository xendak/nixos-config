{ lib, ... }:
let
  paletteFiles = builtins.attrNames (builtins.readDir ./.);
  nixFiles = lib.filter (f: lib.hasSuffix ".nix" f && f != "default.nix") paletteFiles;

  # M3 lazy defaults
  m3_specs = {
    light = {
      primary = "#6750A4";
      on_primary = "#FFFFFF";
      primary_container = "#EADDFF";
      on_primary_container = "#21005D";
      inverse_primary = "#D0BCFF";
      primary_fixed = "#EADDFF";
      primary_fixed_dim = "#D0BCFF";
      on_primary_fixed = "#21005D";
      on_primary_fixed_variant = "#4F378B";

      secondary = "#625B71";
      on_secondary = "#FFFFFF";
      secondary_container = "#E8DEF8";
      on_secondary_container = "#1D192B";
      secondary_fixed = "#E8DEF8";
      secondary_fixed_dim = "#CCC2DC";
      on_secondary_fixed = "#1D192B";
      on_secondary_fixed_variant = "#4A4458";

      tertiary = "#7D5260";
      on_tertiary = "#FFFFFF";
      tertiary_container = "#FFD8E4";
      on_tertiary_container = "#31111D";
      tertiary_fixed = "#FFD8E4";
      tertiary_fixed_dim = "#EFB8C8";
      on_tertiary_fixed = "#31111D";
      on_tertiary_fixed_variant = "#633B48";

      error = "#B3261E";
      on_error = "#FFFFFF";
      error_container = "#F9DEDC";
      on_error_container = "#410E0B";

      background = "#FFFBFE";
      foreground = "#1C1B1F";
      on_background = "#1C1B1F";
      surface = "#FFFBFE";
      on_surface = "#1C1B1F";
      surface_variant = "#E7E0EC";
      on_surface_variant = "#49454F";
      surface_dim = "#DED8E1";
      surface_bright = "#FFFBFE";
      surface_container_lowest = "#FFFFFF";
      surface_container_low = "#F7F2FA";
      surface_container = "#F3EDF7";
      surface_container_high = "#ECE6F0";
      surface_container_highest = "#E6E0E9";

      outline = "#79747E";
      outline_variant = "#CAC4D0";
      inverse_surface = "#313033";
      inverse_on_surface = "#F4EFF4";

      surface_tint = "#6750A4";
      shadow = "#000000";
      scrim = "#000000";
      source_color = "#6750A4";

      # default colors for ansi on white
      white = "#fbf1c7";
      red = "#cc241d";
      green = "#98971a";
      yellow = "#d79921";
      blue = "#458588";
      magenta = "#b16286";
      cyan = "#689d6a";
      gray = "#7c6f64";
      orange = "#d65d0e";
      black = "#3c3836";

      brightwhite = "#928374";
      brightred = "#9d0006";
      brightgreen = "#79740e";
      brightyellow = "#b57614";
      brightblue = "#076678";
      brightmagenta = "#8f3f71";
      brightcyan = "#427b58";
      brightgray = "#928374";
      brightorange = "#af3a03";
      brightblack = "#7c6f64";
    };

    dark = {
      primary = "#D0BCFF";
      on_primary = "#381E72";
      primary_container = "#4F378B";
      on_primary_container = "#EADDFF";
      inverse_primary = "#6750A4";
      primary_fixed = "#EADDFF";
      primary_fixed_dim = "#D0BCFF";
      on_primary_fixed = "#21005D";
      on_primary_fixed_variant = "#4F378B";

      secondary = "#CCC2DC";
      on_secondary = "#332D41";
      secondary_container = "#4A4458";
      on_secondary_container = "#E8DEF8";
      secondary_fixed = "#E8DEF8";
      secondary_fixed_dim = "#CCC2DC";
      on_secondary_fixed = "#1D192B";
      on_secondary_fixed_variant = "#4A4458";

      tertiary = "#EFB8C8";
      on_tertiary = "#492532";
      tertiary_container = "#633B48";
      on_tertiary_container = "#FFD8E4";
      tertiary_fixed = "#FFD8E4";
      tertiary_fixed_dim = "#EFB8C8";
      on_tertiary_fixed = "#31111D";
      on_tertiary_fixed_variant = "#633B48";

      error = "#F2B8B5";
      on_error = "#601410";
      error_container = "#8C1D18";
      on_error_container = "#F9DEDC";

      background = "#1C1B1F";
      on_background = "#E6E1E5";
      foreground = "#E6E1E5";
      surface = "#1C1B1F";
      on_surface = "#E6E1E5";
      surface_variant = "#49454F";
      on_surface_variant = "#CAC4D0";
      surface_dim = "#141218";
      surface_bright = "#3B383E";
      surface_container_lowest = "#0F0D13";
      surface_container_low = "#1D1B20";
      surface_container = "#211F26";
      surface_container_high = "#2B2930";
      surface_container_highest = "#36343B";

      outline = "#938F99";
      outline_variant = "#49454F";
      inverse_surface = "#E6E1E5";
      inverse_on_surface = "#313033";

      surface_tint = "#D0BCFF";
      shadow = "#000000";
      scrim = "#000000";
      source_color = "#6750A4";

      # default colors for ansi on white
      white = "#3c3836";
      red = "#cc241d";
      green = "#98971a";
      yellow = "#d79921";
      blue = "#458588";
      magenta = "#b16286";
      cyan = "#689d6a";
      gray = "#a89984";
      orange = "#d65d0e";
      black = "#ebdbb2";

      brightwhite = "#504945";
      brightred = "#fb4934";
      brightgreen = "#b8bb26";
      brightyellow = "#fabd2f";
      brightblue = "#83a598";
      brightmagenta = "#d3869b";
      brightcyan = "#8ec07c";
      brightgray = "#928374";
      brightorange = "#fe8019";
      brightblack = "#fbf1c7";
    };
  };

  # base16 → M3 (legacy themes)
  base16ToM3 = p: {
    primary = p.base0B;
    on_primary = p.base00;
    primary_container = p.base02;
    on_primary_container = p.base05;
    inverse_primary = p.base0C;
    primary_fixed = p.base0B;
    primary_fixed_dim = p.base0B;
    on_primary_fixed = p.base00;
    on_primary_fixed_variant = p.base02;

    secondary = p.base0D;
    on_secondary = p.base00;
    secondary_container = p.base01;
    on_secondary_container = p.base04;
    secondary_fixed = p.base0D;
    secondary_fixed_dim = p.base0D;
    on_secondary_fixed = p.base00;
    on_secondary_fixed_variant = p.base01;

    tertiary = p.base0A;
    on_tertiary = p.base00;
    tertiary_container = p.base03;
    on_tertiary_container = p.base05;
    tertiary_fixed = p.base0A;
    tertiary_fixed_dim = p.base0A;
    on_tertiary_fixed = p.base00;
    on_tertiary_fixed_variant = p.base03;

    error = p.base08;
    on_error = p.base07;
    error_container = p.base08;
    on_error_container = p.base08;

    background = p.base00;
    on_background = p.base05;
    surface = p.base00;
    on_surface = p.base05;
    surface_variant = p.base01;
    on_surface_variant = p.base04;
    surface_dim = p.base00;
    surface_bright = p.base02;
    surface_container_lowest = p.base00;
    surface_container_low = p.base01;
    surface_container = p.base01;
    surface_container_high = p.base02;
    surface_container_highest = p.base03;

    outline = p.base03;
    outline_variant = p.base02;
    inverse_surface = p.base05;
    inverse_on_surface = p.base00;

    surface_tint = p.base0B;
    shadow = "#000000";
    scrim = "#000000";
    source_color = p.base0B;
  };

  # M3 → base16/ansi
  deriveBase16AndAnsi =
    m3: extra:
    let
      ansi = {
        white = extra.white or m3.on_surface;
        grey = extra.grey or m3.outline;
        red = extra.red or m3.error;
        green = extra.green or m3.primary;
        yellow = extra.yellow or m3.tertiary;
        blue = extra.blue or m3.secondary;
        magenta = extra.magenta or m3.tertiary;
        cyan = extra.cyan or m3.primary_container;
        orange = extra.orange or m3.tertiary;
        black = extra.black or m3.surface;

        brightwhite = extra.brightwhite or ansi.white;
        brightgrey = extra.brightgrey or ansi.grey;
        brightred = extra.brightred or ansi.red;
        brightgreen = extra.brightgreen or ansi.green;
        brightyellow = extra.brightyellow or ansi.yellow;
        brightblue = extra.brightblue or ansi.blue;
        brightmagenta = extra.brightmagenta or ansi.magenta;
        brightcyan = extra.brightcyan or ansi.cyan;
        brightorange = extra.brightorange or ansi.orange;
        brightblack = extra.brightblack or ansi.black;

        keywords = extra.keywords or ansi.red;
        labels = extra.labels or ansi.brightred;
        strings = extra.strings or ansi.green;
        alt_functions = extra.alt_functions or ansi.brightgreen;
        builtins = extra.builtins or ansi.yellow;
        types = extra.types or ansi.brightyellow;
        functions = extra.functions or ansi.blue;
        macros = extra.macros or ansi.brightblue;
        specials = extra.specials or ansi.magenta;
        constants = extra.constants or ansi.brightmagenta;
        modules = extra.modules or ansi.cyan;
        tags = extra.tags or ansi.brightcyan;
        numeric = extra.numeric or ansi.orange;
        punctuation = extra.punctuation or ansi.brightorange;
        comments = extra.comments or ansi.grey;
        inlay = extra.inlay or ansi.brightgrey;
      };
      base16 = {
        base00 = extra.base00 or m3.background;
        base01 = extra.base01 or m3.surface_container;
        base02 = extra.base02 or m3.surface_container_high;
        base03 = extra.base03 or m3.outline;
        base04 = extra.base04 or m3.on_surface_variant;
        base05 = extra.base05 or m3.on_surface;
        base06 = extra.base06 or m3.on_surface;
        base07 = extra.base07 or ansi.white;
        base08 = extra.base08 or ansi.red;
        base09 = extra.base09 or ansi.orange;
        base0A = extra.base0A or ansi.yellow;
        base0B = extra.base0B or ansi.green;
        base0C = extra.base0C or ansi.cyan;
        base0D = extra.base0D or ansi.blue;
        base0E = extra.base0E or ansi.magenta;
        base0F = extra.base0F or m3.outline_variant;
      };
    in
    ansi // base16;

  # process themes for: legacy or new (lazy or not)
  processTheme =
    raw:
    let
      type = raw.type or "dark";

      isLegacy = raw._isLegacy or false;
      # new but not lazyz
      isM3NotLazy = !isLegacy && (raw ? must);
      # new and lazy (else)
      # isLazyM3 = !isStatic && !isM3First && (raw ? palette);
    in

    # legacy
    if isLegacy then
      let
        p = raw.palette;
        m3 = base16ToM3 p // (lib.filterAttrs (_: v: v != null) (raw.overrides or { }));
        b16 = deriveBase16AndAnsi m3 p;
        finalPalette =
          m3
          // b16
          // p
          // {
            bg = p.bg or p.base00;
            fg = p.fg or p.base05;
            foreground = p.fg;
            selection_bg = p.selection_bg or m3.surface_container_high;
            selection_fg = p.selection_fg or m3.on_surface_variant;
          };
      in
      raw
      // {
        palette = finalPalette;
        m3 = m3;
      }

    # new not lazy
    else if isM3NotLazy then
      let
        must = raw.must;
        overrides = lib.filterAttrs (_: v: v != null) (raw.overrides or { });

        m3_derived = {
          background = must.bg;
          on_background = must.fg;
          surface = must.bg;
          on_surface = must.fg;
          foreground = must.fg;
          surface_variant = must.surface_container_high;
          on_surface_variant = must.fg;
          surface_dim = must.bg;
          surface_bright = must.surface_container_high;
          surface_container_lowest = must.bg;
          surface_container_low = must.surface_container;
          surface_container = must.surface_container;
          surface_container_high = must.surface_container_high;
          surface_container_highest = must.surface_container_high;

          primary = must.primary;
          on_primary = must.bg;
          primary_container = must.surface_container_high;
          on_primary_container = must.primary;
          inverse_primary = must.bg;
          primary_fixed = must.primary;
          primary_fixed_dim = must.primary;
          on_primary_fixed = must.bg;
          on_primary_fixed_variant = must.surface_container_high;

          secondary = must.secondary;
          on_secondary = must.bg;
          secondary_container = must.surface_container;
          on_secondary_container = must.secondary;
          secondary_fixed = must.secondary;
          secondary_fixed_dim = must.secondary;
          on_secondary_fixed = must.bg;
          on_secondary_fixed_variant = must.surface_container;

          tertiary = must.tertiary;
          on_tertiary = must.bg;
          tertiary_container = must.surface_container;
          on_tertiary_container = must.tertiary;
          tertiary_fixed = must.tertiary;
          tertiary_fixed_dim = must.tertiary;
          on_tertiary_fixed = must.bg;
          on_tertiary_fixed_variant = must.surface_container;

          error = must.error;
          on_error = must.bg;
          error_container = must.surface_container;
          on_error_container = must.error;

          outline = must.fg;
          outline_variant = must.surface_container_high;
          inverse_surface = must.fg;
          inverse_on_surface = must.bg;

          shadow = "#000000";
          scrim = "#000000";
          surface_tint = must.primary;
          source_color = must.primary;
        };

        m3 = m3_derived // overrides;
        b16 = deriveBase16AndAnsi m3 overrides;

        finalPalette =
          m3
          // b16
          // {
            bg = must.bg;
            fg = must.fg;
            cursor_bg = must.cursor_bg or must.primary;
            cursor_fg = must.cursor_fg or must.bg;
            selection_bg = overrides.selection_bg or m3.surface_container_high;
            selection_fg = overrides.selection_fg or m3.on_surface_variant;
          };
      in
      raw
      // {
        palette = finalPalette;
        m3 = m3;
      }

    # new and lazy
    else
      let
        baseSpec = m3_specs.${type};
        overrides = lib.filterAttrs (_: v: v != null) (raw.overrides or { });
        m3 = baseSpec // (raw.palette or { }) // overrides;
        b16 = deriveBase16AndAnsi m3 (raw.palette or { } // overrides);

        finalPalette =
          m3
          // b16
          // {
            cursor_bg = raw.palette.cursor_bg or m3.primary;
            cursor_fg = raw.palette.cursor_fg or m3.on_primary;
            selection_bg = overrides.selection_bg or m3.surface_container_high;
            selection_fg = overrides.selection_fg or m3.on_surface_variant;
          };
      in
      raw
      // {
        palette = finalPalette;
        m3 = m3;
      };

  # load files
  toAttrSet =
    file:
    let
      name = lib.removeSuffix ".nix" file;
      imported = import ./${file};
      rawData = if builtins.isFunction imported then imported { inherit lib; } else imported;
      isLegacy = !(builtins.isFunction imported) && (rawData ? palette) && (rawData.palette ? base00);
    in
    {
      inherit name;
      value = processTheme (rawData // { _isLegacy = isLegacy; });
    };
in
lib.listToAttrs (map toAttrSet nixFiles)
