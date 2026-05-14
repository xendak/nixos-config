{
  paletteSet,
  lib,
  ...
}:
let
  p = paletteSet.palette;
  colorList = [
    (p.surface_container) # x0
    (p.error) # x1
    (p.primary) # x2
    (p.secondary) # x3
    (p.blue) # x4
    (p.magenta) # x5
    (p.cyan) # x6
    (p.on_background) # x7
    (p.outline) # x8
    (p.error_container) # x9
    (p.primary_container) # x10
    (p.secondary_container) # x11
    (p.blue) # x12
    (p.tertiary_container) # x13
    (p.primary_fixed) # x14
    (p.on_surface_variant) # x15
  ];
  yuckColorArray = "[${lib.concatMapStringsSep ", " (c: ''\"${c}\"'') colorList}]";
in
{
  "eww/colors.scss" = ''
    $xbg: ${p.background};
    $xfg: ${p.on_background};
    $x0: ${p.surface_container};
    $x8: ${p.outline}; 
    $x15: ${p.on_surface_variant};

    $x1: ${p.red};
    $x2: ${p.primary};
    $x3: ${p.secondary};
    $x4: ${p.blue};
    $x5: ${p.magenta};
    $x6: ${p.cyan};

    $x7: ${p.on_background};
    $x9: ${p.error_container};
    $x10: ${p.primary_container};
    $x11: ${p.secondary_container};
    $x12: ${p.green};
    $x13: ${p.tertiary_container};
    $x14: ${p.primary_fixed};
  '';

  "eww/colors.yuck" = ''
    (defvar colors "${yuckColorArray}")
  '';

}

# x1->x6
# Red / Alarms / Delete
# Primary / Workspaces / Success
# Secondary / Warning / Orange
# Blue / Info / Network
# Purple / Magenta / Media
# Sky Blue / Highlight
# default:
# $xbg: "#101319";
# $xfg: "#f4f3ee";
# $x0: "#171b24";
# $x1: "#E34F4F";
# $x2: "#69bfce";
# $x3: "#e37e4f";
# $x4: "#5679E3";
# $x5: "#956dca";
# $x6: "#5599E2";
# $x7: "#f4f3ee";
# $x8: "#3A435A";
# $x9: "#DE2B2B";
# $x10: "#56B7C8";
# $x11: "#DE642B";
# $x12: "#3E66E0";
# $x13: "#885AC4";
# $x14: "#3F8CDE";
# $x15: "#DDDBCF";
