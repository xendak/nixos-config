{ paletteSet, config, ... }:
let
  p = paletteSet.palette;
in
{
  "zathura/zathurarc" = ''
    set selection-clipboard "clipboard"
    set font "${config.fontProfiles.regular.family} 14"
    set recolor "true"
    set default-bg "${p.base00}"
    set default-fg "${p.base05}"
    set statusbar-bg "${p.base02}"
    set statusbar-fg "${p.base04}"
    set inputbar-bg "${p.base00}"
    set inputbar-fg "${p.base07}"
    set highlight-color "${p.base0A}"
    set highlight-active-color "${p.base0D}"
    set recolor-lightcolor "${p.base00}"
    set recolor-darkcolor "${p.base06}"
  '';
}
