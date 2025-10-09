{ paletteSet, ... }:
let
  p = paletteSet.palette;
in
{
  "fcitx5/theme.conf" = ''
    [Metadata]
    Name=current
    [InputPanel]
    NormalColor=${p.fg}
    HighlightCandidateColor=${p.base00}
    HighlightColor=${p.accent}
    [InputPanel/Background]
    Color=${p.bg}
    [InputPanel/Highlight]
    Color=${p.accent}
  '';
  "fcitx5/highlight.svg" = ''
    <svg xmlns="http://www.w3.org/2000/svg" width="39" height="39">
      <rect width="39" height="39" x="0" y="0" fill="${p.accent}" stroke="${p.base02}" stroke-width="0" rx="12"/>
    </svg>
  '';
  "fcitx5/panel.svg" = ''
    <svg xmlns="http://www.w3.org/2000/svg" width="40" height="40">
      <rect width="39" height="39" x=".5" y=".5" fill="${p.base02}" stroke="${p.base00}" rx="12"/>
    </svg>
  '';
}
