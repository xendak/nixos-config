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
      noto-fonts-emoji
      source-han-sans
      source-han-code-jp
      nerd-fonts.jetbrains-mono

      cozette
    ];

    enableDefaultPackages = true;

    fontconfig.defaultFonts = {
      serif = [
        "Sofia Pro"
        "EB Garamond"
        "Source Han Sans JP"
        "Noto Color Emoji"
      ];
      sansSerif = [
        "Sofia Pro"
        "Source Han Sans JP"
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
