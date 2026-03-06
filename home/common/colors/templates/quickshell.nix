{ paletteSet, lib, ... }:
let
  p = paletteSet.palette;
  hex = str: lib.removePrefix "#" str;
  todo = "ffeeaa";
in
{
  "quickshell/theme.txt" = ''
    primary_paletteKeyColor ${hex p.primary}
    secondary_paletteKeyColor ${hex p.secondary}
    tertiary_paletteKeyColor ${hex p.tertiary}
    neutral_paletteKeyColor ${hex p.outline}
    neutral_variant_paletteKeyColor ${hex p.outline_variant}
    background ${hex p.background}
    onBackground ${hex p.on_background}
    surface ${hex p.surface}
    surfaceDim ${hex p.surface_dim}
    surfaceBright ${hex p.surface_bright}
    surfaceContainerLowest ${hex p.surface_container_lowest}
    surfaceContainerLow ${hex p.surface_container_low}
    surfaceContainer ${hex p.surface_container}
    surfaceContainerHigh ${hex p.surface_container_high}
    surfaceContainerHighest ${hex p.surface_container_highest}
    onSurface ${hex p.on_surface}
    surfaceVariant ${hex p.surface_variant}
    onSurfaceVariant ${hex p.on_surface_variant}
    inverseSurface ${hex p.inverse_surface}
    inverseOnSurface ${hex p.inverse_on_surface}
    outline ${hex p.outline}
    outlineVariant ${hex p.outline_variant}
    shadow ${hex p.shadow}
    scrim ${hex p.scrim}
    surfaceTint ${hex p.surface_tint}
    primary ${hex p.primary}
    onPrimary ${hex p.on_primary}
    primaryContainer ${hex p.primary_container}
    onPrimaryContainer ${hex p.on_primary_container}
    inversePrimary ${hex p.inverse_primary}
    secondary ${hex p.secondary}
    onSecondary ${hex p.on_secondary}
    secondaryContainer ${hex p.secondary_container}
    onSecondaryContainer ${hex p.on_secondary_container}
    tertiary ${hex p.tertiary}
    onTertiary ${hex p.on_tertiary}
    tertiaryContainer ${hex p.tertiary_container}
    onTertiaryContainer ${hex p.on_tertiary_container}
    error ${hex p.error}
    onError ${hex p.on_error}
    errorContainer ${hex p.error_container}
    onErrorContainer ${hex p.on_error_container}
    primaryFixed ${hex p.primary_fixed}
    primaryFixedDim ${hex p.primary_fixed_dim}
    onPrimaryFixed ${hex p.on_primary_fixed}
    onPrimaryFixedVariant ${hex p.on_primary_fixed_variant}
    secondaryFixed ${hex p.secondary_fixed}
    secondaryFixedDim ${hex p.secondary_fixed_dim}
    onSecondaryFixed ${hex p.on_secondary_fixed}
    onSecondaryFixedVariant ${hex p.on_secondary_fixed_variant}
    tertiaryFixed ${hex p.tertiary_fixed}
    tertiaryFixedDim ${hex p.tertiary_fixed_dim}
    onTertiaryFixed ${hex p.on_tertiary_fixed}
    onTertiaryFixedVariant ${hex p.on_tertiary_fixed_variant}
    rosewater ${todo}
    flamingo ${todo}
    pink ${hex p.magenta}
    mauve ${hex p.magenta}
    red ${hex p.red}
    maroon ${todo}
    peach ${hex p.orange}
    yellow ${hex p.yellow}
    green ${hex p.green}
    teal ${hex p.cyan}
    sky ${hex p.cyan}
    sapphire ${hex p.blue}
    blue ${hex p.blue}
    lavender ${hex p.magenta}
  '';
}
