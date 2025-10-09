{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;
  hex = str: lib.removePrefix "#" str;
in
{
  "foot/colors.ini" = # ini
    ''
      [colors]
      cursor=${hex p.cursor_fg} ${hex p.cursor_bg}
      foreground=${hex p.fg}
      background=${hex p.bg}
      regular0=${hex p.base00}
      regular1=${hex p.base08}
      regular2=${hex p.base0B}
      regular3=${hex p.base0A}
      regular4=${hex p.base0D}
      regular5=${hex p.base0E}
      regular6=${hex p.base0C}
      regular7=${hex p.base05}
      bright0=${hex p.base03}
      bright1=${hex p.base08}
      bright2=${hex p.base0B}
      bright3=${hex p.base0A}
      bright4=${hex p.base0D}
      bright5=${hex p.base0E}
      bright6=${hex p.base0C}
      bright7=${hex p.base07}
      urls=${hex p.base04}
    '';
}
