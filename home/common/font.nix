{ pkgs, ... }:
{
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = [
        "JetBrainsMono Nerd Font"
        "Source Han Sans JP"
        "Noto Color Emoji"
      ];
      emoji = [
        "Noto Color Emoji"
      ];
      sansSerif = [
        "Sofia Pro"
        "Source Han Sans JP"
        "Noto Color Emoji"
      ];

      serif = [
        "EB Garamond"
        "Source Han Serif JP"
      ];
    };
  };

  home.packages = [
    pkgs.source-han-serif-japanese
    pkgs.source-han-sans-japanese
    pkgs.source-han-code-jp
    pkgs.eb-garamond
    pkgs.anakron
    # pkgs.material-symbols
    pkgs.material-design-icons
    pkgs.ibm-plex
  ];

  fontProfiles = {
    enable = true;
    monospace = {
      family = "JetBrainsMono Nerd Font";
      package = pkgs.nerd-fonts.jetbrains-mono;
    };
    regular = {
      family = "Sofia Pro";
      package = pkgs.useful-fonts;
    };
  };
}
