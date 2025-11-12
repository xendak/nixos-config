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

    fontconfig.localConf = ''
      <!-- use a less horrible font substition for pdfs such as https://www.bkent.net/Doc/mdarchiv.pdf -->
      <match target="pattern">
        <test qual="any" name="family"><string>Arial</string></test>
        <edit name="family" mode="assign" binding="same"><string>IBM Plex Sans</string></edit>
      </match>
    '';
    fontconfig.defaultFonts = {
      serif = [
        "IBM Plex Sans"
        "Source Han Sans"
        "Noto Color Emoji"
      ];
      sansSerif = [
        "Sofia Pro"
        "IBM Plex Sans"
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
