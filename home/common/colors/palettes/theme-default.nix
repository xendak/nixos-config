{ lib, ... }:
{
  light = {
    primary = "#6750A4";
    on_primary = "#FFFFFF";
    primary_container = "#EADDFF";
    on_primary_container = "#21005D";
    inverse_primary = "#D0BCFF";
    primary_fixed = "#EADDFF";
    primary_fixed_dim = "#D0BCFF";
    on_primary_fixed = "#21005D";
    on_primary_fixed_variant = "#4F378B";

    secondary = "#625B71";
    on_secondary = "#FFFFFF";
    secondary_container = "#E8DEF8";
    on_secondary_container = "#1D192B";
    secondary_fixed = "#E8DEF8";
    secondary_fixed_dim = "#CCC2DC";
    on_secondary_fixed = "#1D192B";
    on_secondary_fixed_variant = "#4A4458";

    tertiary = "#7D5260";
    on_tertiary = "#FFFFFF";
    tertiary_container = "#FFD8E4";
    on_tertiary_container = "#31111D";
    tertiary_fixed = "#FFD8E4";
    tertiary_fixed_dim = "#EFB8C8";
    on_tertiary_fixed = "#31111D";
    on_tertiary_fixed_variant = "#633B48";

    error = "#B3261E";
    on_error = "#FFFFFF";
    error_container = "#F9DEDC";
    on_error_container = "#410E0B";

    background = "#FFFBFE";
    foreground = "#1C1B1F";
    bg = "#FFFBFE";
    fg = "#1C1B1F";

    on_background = "#1C1B1F";
    surface = "#FFFBFE";
    on_surface = "#1C1B1F";
    surface_variant = "#E7E0EC";
    on_surface_variant = "#49454F";
    surface_dim = "#DED8E1";
    surface_bright = "#FFFBFE";
    surface_container_lowest = "#FFFFFF";
    surface_container_low = "#F7F2FA";
    surface_container = "#F3EDF7";
    surface_container_high = "#ECE6F0";
    surface_container_highest = "#E6E0E9";

    dim = "#808080";
    outline = "#79747E";
    outline_variant = "#CAC4D0";
    inverse_surface = "#313033";
    inverse_on_surface = "#F4EFF4";

    surface_tint = "#6750A4";
    shadow = "#000000";
    scrim = "#000000";
    source_color = "#6750A4";

    # default colors for ansi on white
    white = "#fbf1c7";
    red = "#cc241d";
    green = "#98971a";
    yellow = "#d79921";
    blue = "#458588";
    magenta = "#b16286";
    cyan = "#689d6a";
    gray = "#7c6f64";
    orange = "#d65d0e";
    black = "#3c3836";

    brightwhite = "#928374";
    brightred = "#9d0006";
    brightgreen = "#79740e";
    brightyellow = "#b57614";
    brightblue = "#076678";
    brightmagenta = "#8f3f71";
    brightcyan = "#427b58";
    brightgray = "#928374";
    brightorange = "#af3a03";
    brightblack = "#7c6f64";
  };

  dark = {
    primary = "#D0BCFF";
    on_primary = "#381E72";
    primary_container = "#4F378B";
    on_primary_container = "#EADDFF";
    inverse_primary = "#6750A4";
    primary_fixed = "#EADDFF";
    primary_fixed_dim = "#D0BCFF";
    on_primary_fixed = "#21005D";
    on_primary_fixed_variant = "#4F378B";

    secondary = "#CCC2DC";
    on_secondary = "#332D41";
    secondary_container = "#4A4458";
    on_secondary_container = "#E8DEF8";
    secondary_fixed = "#E8DEF8";
    secondary_fixed_dim = "#CCC2DC";
    on_secondary_fixed = "#1D192B";
    on_secondary_fixed_variant = "#4A4458";

    tertiary = "#EFB8C8";
    on_tertiary = "#492532";
    tertiary_container = "#633B48";
    on_tertiary_container = "#FFD8E4";
    tertiary_fixed = "#FFD8E4";
    tertiary_fixed_dim = "#EFB8C8";
    on_tertiary_fixed = "#31111D";
    on_tertiary_fixed_variant = "#633B48";

    error = "#F2B8B5";
    on_error = "#601410";
    error_container = "#8C1D18";
    on_error_container = "#F9DEDC";

    background = "#1C1B1F";
    foreground = "#E6E1E5";
    bg = "#1C1B1F";
    fg = "#E6E1E5";

    on_background = "#E6E1E5";
    surface = "#1C1B1F";
    on_surface = "#E6E1E5";
    surface_variant = "#49454F";
    on_surface_variant = "#CAC4D0";
    surface_dim = "#141218";
    surface_bright = "#3B383E";
    surface_container_lowest = "#0F0D13";
    surface_container_low = "#1D1B20";
    surface_container = "#211F26";
    surface_container_high = "#2B2930";
    surface_container_highest = "#36343B";

    dim = "#49454f";
    outline = "#938F99";
    outline_variant = "#49454F";
    inverse_surface = "#E6E1E5";
    inverse_on_surface = "#313033";

    surface_tint = "#D0BCFF";
    shadow = "#000000";
    scrim = "#000000";
    source_color = "#6750A4";

    black = "#353535";
    red = "#C4746E";
    green = "#8A9A7B";
    yellow = "#C4B28A";
    blue = "#8BA4B0";
    magenta = "#A292A3";
    cyan = "#8EA4A2";
    white = "#C8C093";
    gray = "#a89984";
    orange = "#d65d0e";

    brightblack = "#454545";
    brightred = "#E46876";
    brightgreen = "#87A987";
    brightyellow = "#E6C384";
    brightblue = "#7FB4CA";
    brightmagenta = "#938AA9";
    brightcyan = "#7AA89F";
    brightwhite = "#A6A69C";
    brightgray = "#928374";
    brightorange = "#fe8019";
  };
}
