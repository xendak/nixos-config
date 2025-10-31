{ pkgs, ... }:
{
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = [
        "JetBrainsMono Nerd Font"
        "Source Han Sans"
        "Noto Color Emoji"
      ];
      emoji = [
        "Noto Color Emoji"
      ];
      sansSerif = [
        "Sofia Pro"
        "Source Han Sans"
        "Noto Color Emoji"
      ];

      serif = [
        "Vollkorn"
        "EB Garamond"
        "Source Han Serif"
      ];
    };
  };

  home.packages = [
    pkgs.source-han-serif
    pkgs.source-han-sans
    pkgs.source-han-code-jp
    pkgs.eb-garamond
    pkgs.anakron
    pkgs.vollkorn
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
