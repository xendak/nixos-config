{ paletteSet, lib, ... }:
let
  hex = str: lib.removePrefix "#" str;
  p = paletteSet.palette;
in
{
  "foot/colors.ini" =
    # ini
    ''
      [colors]
      cursor=${hex (p.cursor_fg or p.on_surface)} ${hex (p.cursor_bg or p.primary)}
      foreground=${hex p.foreground}
      background=${hex p.background}

      regular0=${hex p.surface_container_high}
      bright0=${hex p.outline}

      regular1=${hex p.error}
      bright1=${hex p.error}

      regular2=${hex p.primary}
      bright2=${hex p.primary}

      regular3=${hex p.tertiary}
      bright3=${hex p.tertiary}

      regular4=${hex p.secondary}
      bright4=${hex p.secondary}

      regular5=${hex p.on_secondary_container}
      bright5=${hex p.on_secondary_container}

      regular6=${hex p.on_primary_container}
      bright6=${hex p.on_primary_container}

      regular7=${hex p.on_surface}
      bright7=${hex p.inverse_on_surface}

      urls=${hex p.secondary}
    '';
}
