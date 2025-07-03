{ config, ... }:
let
  mkZathuraTheme =
    { palette, fonts }:
    with palette;
    ''
      set selection-clipboard     "clipboard"
      set font                    "${fonts.regular.family} 12"
      set recolor                 "true"

      # Theming
      set default-bg              "${base00}"
      set default-fg              "${base05}"
      set statusbar-bg            "${base02}"
      set statusbar-fg            "${base04}"
      set inputbar-bg             "${base00}"
      set inputbar-fg             "${base07}"
      set notification-bg         "${base00}"
      set notification-fg         "${base07}"
      set notification-error-bg   "${base00}"
      set notification-error-fg   "${base08}"
      set notification-warning-bg "${base00}"
      set notification-warning-fg "${base08}"
      set highlight-color         "${base0A}"
      set highlight-active-color  "${base0D}"
      set completion-bg           "${base01}"
      set completion-fg           "${base05}"
      set completion-highlight-bg "${base0D}"
      set completion-highlight-fg "${base07}"
      set recolor-lightcolor      "${base00}"
      set recolor-darkcolor       "${base06}"
    '';
in
{
  xdg.mimeApps.defaultApplications = {
    "application/pdf" = [ "org.pwmt.zathura.desktop" ];
  };

  programs.zathura.enable = true;

  home.file = {
    ".config/themes/zathura-default".text = mkZathuraTheme {
      palette = config.themes.default.colorScheme.palette;
      fonts = config.fontProfiles;
    };
    ".config/themes/zathura-dark".text = mkZathuraTheme {
      palette = config.themes.dark.colorScheme.palette;
      fonts = config.fontProfiles;
    };
    ".config/themes/zathura-light".text = mkZathuraTheme {
      palette = config.themes.light.colorScheme.palette;
      fonts = config.fontProfiles;
    };
  };
}
