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
      cursor=${hex p.cursor_fg} ${hex p.cursor_bg}
      foreground=${hex p.foreground}
      background=${hex p.background}

      regular0=${hex p.black}
      bright0=${hex p.brightblack}

      regular1=${hex p.red}
      bright1=${hex p.brightred}

      regular2=${hex p.green}
      bright2=${hex p.brightgreen}

      regular3=${hex p.yellow}
      bright3=${hex p.brightyellow}

      regular4=${hex p.blue}
      bright4=${hex p.brightblue}

      regular5=${hex p.magenta}
      bright5=${hex p.brightmagenta}

      regular6=${hex p.cyan}
      bright6=${hex p.brightcyan}

      regular7=${hex p.white}
      bright7=${hex p.brightwhite}

      urls=${hex p.secondary}
    '';
}
