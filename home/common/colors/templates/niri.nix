{
  paletteSet,
  ...
}:
let
  p = paletteSet.palette;
in
{
  "niri/colors.kdl" =
    # toml
    ''
      layout {
          focus-ring {
              active-gradient angle=135 from="${p.primary}50" in="oklch longer hue" relative-to="window" to="${p.shadow}50"
          }
      border {
          active-color "${p.primary}"
          inactive-color "${p.shadow}"
      }

          shadow {
              color "${p.primary}"
              inactive-color "${p.shadow}"
          }
          tab-indicator {
              active-gradient angle=45 from="${p.secondary}" in="oklch shorter hue" relative-to="window" to="${p.shadow}"
              inactive-color "${p.shadow}50"
          }
          insert-hint { gradient angle=135 from="${p.tertiary}" in="oklch shorter hue" relative-to="window" to="${p.shadow}"; }
      }
            
    '';
}
