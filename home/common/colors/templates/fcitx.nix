{ paletteSet, ... }:
let
  m = paletteSet.palette;
in
{
  "fcitx5/theme.conf" = ''
    [Metadata]
    Name=current

    [InputPanel]
    NormalColor=${m.on_surface}
    HighlightCandidateColor=${m.on_primary}
    HighlightColor=${m.primary}

    [InputPanel/Background]
    Color=${m.surface_container}

    [InputPanel/Highlight]
    Color=${m.primary}
  '';

  "fcitx5/highlight.svg" = ''
    <svg xmlns="http://www.w3.org/2000/svg" width="39" height="39">
      <rect width="39" height="39" x="0" y="0" fill="${m.primary}" stroke="${m.primary_container}" stroke-width="0" rx="12"/>
    </svg>
  '';

  "fcitx5/panel.svg" = ''
    <svg xmlns="http://www.w3.org/2000/svg" width="40" height="40">
      <rect width="39" height="39" x=".5" y=".5" fill="${m.surface_container}" stroke="${m.outline_variant}" rx="12"/>
    </svg>
  '';
}
