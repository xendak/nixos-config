# Must for now =>
```nix
must = {
  primary = "#8FCFF3";
  secondary = "#B5C9D7";
  tertiary = "#C9C1EA";
  error = "#FFB4AB";

  foreground = "#DFE3E7";

  background = "#0F1417";
  surface_container = "#1C2023";
  surface_container_high = "#262B2E";

  cursor_bg = "#e3556f";
  cursor_fg = "#101010";

  # for now
  bg = must.background;
  fg = must.foreground;
  accent = must.primary;
};
```


# ALL OVERRIDES Material 3 Spec
```nix
overrides = {
# Primary roles
  primary = null;
  on_primary = null;
  primary_container = null;
  on_primary_container = null;
  inverse_primary = null;
  primary_fixed = null;
  primary_fixed_dim = null;
  on_primary_fixed = null;
  on_primary_fixed_variant = null;

  # Secondary roles
  secondary = null;
  on_secondary = null;
  secondary_container = null;
  on_secondary_container = null;
  secondary_fixed = null;
  secondary_fixed_dim = null;
  on_secondary_fixed = null;
  on_secondary_fixed_variant = null;

  # Tertiary roles
  tertiary = null;
  on_tertiary = null;
  tertiary_container = null;
  on_tertiary_container = null;
  tertiary_fixed = null;
  tertiary_fixed_dim = null;
  on_tertiary_fixed = null;
  on_tertiary_fixed_variant = null;

  # Error roles
  error = null;
  on_error = null;
  error_container = null;
  on_error_container = null;

  # Surface & Background
  background = null;
  on_background = null;
  surface = null;
  on_surface = null;
  surface_variant = null;
  on_surface_variant = null;
  surface_dim = null;
  surface_bright = null;
  surface_container_lowest = null;
  surface_container_low = null;
  surface_container = null;
  surface_container_high = null;
  surface_container_highest = null;

  # Outlines & Inverse
  outline = null;
  outline_variant = null;
  inverse_surface = null;
  inverse_on_surface = null;

  # Utilities
  surface_tint = null;
  shadow = null;
  scrim = null;
  source_color = null;
}
```
