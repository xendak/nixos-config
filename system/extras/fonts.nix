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
      miracode

      cozette
    ];

    enableDefaultPackages = true;

    fontconfig.localConf = ''
      <!-- use a less horrible font substition for pdfs such as https://www.bkent.net/Doc/mdarchiv.pdf -->
      <match target="pattern">
        <test qual="any" name="family"><string>Arial</string></test>
        <edit name="family" mode="assign" binding="same"><string>Sofia Pro</string></edit>
      </match>
    '';
    fontconfig.defaultFonts = {
      serif = [
        "Sofia Pro"
        "EB Garamond"
        "Source Han Serif"
        "Noto Color Emoji"
      ];
      sansSerif = [
        "Sofia Pro"
        "EB Garamond"
        "IBM Plex Sans"
        "Source Han Sans"
        "Noto Color Emoji"
      ];
      monospace = [
        "JetBrainsMono Nerd Font"
        "CozzeteVector"
        "Source Han Code JP"
        "Noto Color Emoji"
      ];
      emoji = [ "Noto Color Emoji" ];
    };
  };
}
