{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;
  hex = str: lib.removePrefix "#" str;
in
{
  "quickshell/theme.txt" = ''
    primary_paletteKeyColor ${hex p.accent}
    secondary_paletteKeyColor ${hex p.base0E}
    tertiary_paletteKeyColor ${hex p.base0B}
    neutral_paletteKeyColor ${hex p.base04}
    neutral_variant_paletteKeyColor ${hex p.base03}
    background ${hex p.base01}
    onBackground ${hex p.base05}
    surface ${hex p.base00}
    surfaceDim ${hex p.base00}
    surfaceBright ${hex p.base02}
    surfaceContainerLowest ${hex p.base00}
    surfaceContainerLow ${hex p.base01}
    surfaceContainer ${hex p.base02}
    surfaceContainerHigh ${hex p.base03}
    surfaceContainerHighest ${hex p.base04}
    onSurface ${hex p.base05}
    surfaceVariant ${hex p.base03}
    onSurfaceVariant ${hex p.base06}
    inverseSurface ${hex p.base06}
    inverseOnSurface ${hex p.base00}
    outline ${hex p.base04}
    outlineVariant ${hex p.base03}
    shadow 000000
    scrim 000000
    surfaceTint ${hex p.base0D}
    primary ${hex p.accent}
    onPrimary ${hex p.base00}
    primaryContainer ${hex p.base02}
    onPrimaryContainer ${hex p.base06}
    inversePrimary ${hex p.base0C}
    secondary ${hex p.base0E}
    onSecondary ${hex p.base00}
    secondaryContainer ${hex p.base03}
    onSecondaryContainer ${hex p.base06}
    tertiary ${hex p.base0B}
    onTertiary ${hex p.base00}
    tertiaryContainer ${hex p.base04}
    onTertiaryContainer ${hex p.base06}
    error ${hex p.base08}
    onError ${hex p.base00}
    errorContainer ${hex p.base02}
    onErrorContainer ${hex p.base08}
    primaryFixed ${hex p.accent}
    primaryFixedDim ${hex p.base0C}
    onPrimaryFixed ${hex p.base00}
    onPrimaryFixedVariant ${hex p.base02}
    secondaryFixed ${hex p.base0E}
    secondaryFixedDim ${hex p.base0F}
    onSecondaryFixed ${hex p.base00}
    onSecondaryFixedVariant ${hex p.base03}
    tertiaryFixed ${hex p.base0B}
    tertiaryFixedDim ${hex p.base0A}
    onTertiaryFixed ${hex p.base00}
    onTertiaryFixedVariant ${hex p.base02}
    rosewater ${hex p.base08}
    flamingo ${hex p.base08}
    pink ${hex p.base0E}
    mauve ${hex p.base0E}
    red ${hex p.base08}
    maroon ${hex p.base0F}
    peach ${hex p.base09}
    yellow ${hex p.base0A}
    green ${hex p.base0B}
    teal ${hex p.base0C}
    sky ${hex p.base0D}
    sapphire ${hex p.base0D}
    blue ${hex p.base0D}
    lavender ${hex p.base0E}
  '';
}
