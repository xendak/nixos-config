{ lib, ... }:
let
  paletteFiles = builtins.attrNames (builtins.readDir ./.);
  nixFiles = lib.filter (
    f: lib.hasSuffix ".nix" f && f != "default.nix" && f != "colors.nix" && f != "theme-default.nix"
  ) paletteFiles;

  colorLib = import ./colors.nix { inherit lib; };
  # Derives fixed, fixed_dim, and bright for a single role.
  getFixedColors =
    type: baseColor: prefix: overrides:
    let
      tones = colorLib.toneSet type baseColor;
    in
    {
      "${prefix}_fixed" = overrides."${prefix}_fixed" or tones.fixed;
      "${prefix}_fixed_dim" = overrides."${prefix}_fixed_dim" or tones.fixed_dim;
    };

  m3_specs = import ./theme-default.nix { inherit lib; };

  # M3 → ansi
  deriveAnsi =
    type: m3: extra:
    let
      bright = color: colorLib.toneSet type color;

      ansi = {
        white = extra.white or m3.on_surface;
        gray = extra.gray or m3.outline;
        red = extra.red or m3.error;
        green = extra.green or m3.primary;
        yellow = extra.yellow or m3.tertiary;
        blue = extra.blue or m3.secondary;
        magenta = extra.magenta or m3.tertiary;
        cyan = extra.cyan or m3.primary_container;
        orange = extra.orange or m3.tertiary;
        black = extra.black or m3.surface;

        brightwhite = extra.brightwhite or (bright ansi.white).bright;
        brightgray = extra.brightgray or (bright ansi.gray).bright;
        brightred = extra.brightred or (bright ansi.red).bright;
        brightgreen = extra.brightgreen or (bright ansi.green).bright;
        brightyellow = extra.brightyellow or (bright ansi.yellow).bright;
        brightblue = extra.brightblue or (bright ansi.blue).bright;
        brightmagenta = extra.brightmagenta or (bright ansi.magenta).bright;
        brightcyan = extra.brightcyan or (bright ansi.cyan).bright;
        brightorange = extra.brightorange or (bright ansi.orange).bright;
        brightblack = extra.brightblack or (bright ansi.black).bright;

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
        comments = extra.comments or ansi.gray;
        inlay = extra.inlay or ansi.comments;
      };
    in
    ansi;

  processTheme =
    raw:
    let
      type = raw.type or "dark";
    in
    # :NotLazy
    if !raw.isLazy then
      let
        must = raw.must;
        overrides = lib.filterAttrs (_: v: v != null) (raw.overrides or { });
        fixedVariants =
          (getFixedColors type must.primary "primary" overrides)
          // (getFixedColors type must.secondary "secondary" overrides)
          // (getFixedColors type must.tertiary "tertiary" overrides);

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

          outline = must.dim;
          outline_variant = must.surface_container_high;
          inverse_surface = must.fg;
          inverse_on_surface = must.bg;

          shadow = "#000000";
          scrim = "#000000";
          surface_tint = must.primary;
          source_color = must.primary;
        }
        // fixedVariants;

        m3 = m3_derived // overrides;
        ansi = deriveAnsi type m3 overrides;

        finalPalette =
          m3
          // ansi
          // {
            dim = must.dim;
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
        ansi = deriveAnsi type m3 (raw.palette or { } // overrides);

        finalPalette =
          m3
          // ansi
          // {
            dim = m3.dim;
            cursor_bg = m3.cursor_bg or m3.primary;
            cursor_fg = m3.cursor_fg or m3.on_primary;
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
    in
    {
      inherit name;
      value = processTheme rawData;
    };
in
lib.listToAttrs (map toAttrSet nixFiles)
