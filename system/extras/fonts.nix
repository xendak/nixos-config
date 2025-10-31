{ pkgs, ... }:
{
  fonts = {
    packages = with pkgs; [
      material-symbols
      material-design-icons
      material-icons
      font-awesome_6
      ibm-plex
      # font-awesome
      # noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
      source-han-sans
      source-han-code-jp
      nerd-fonts.jetbrains-mono

      vollkorn
      cozette
    ];

    enableDefaultPackages = true;

    fontconfig.defaultFonts = {
      serif = [
        "Sofia Pro"
        "Vollkorn"
        "Source Han Sans"
        "Noto Color Emoji"
      ];
      sansSerif = [
        "Sofia Pro"
        "Vollkorn"
        "Source Han Sans"
        "Noto Color Emoji"
      ];
      monospace = [
        "JetBrainsMono Nerd Font"
        "Scientifica"
        "Source Han Code JP"
        "Noto Color Emoji"
      ];
      emoji = [ "Noto Color Emoji" ];
    };
  };
}
