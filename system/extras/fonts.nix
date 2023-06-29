{ lib, config, inputs, pkgs, ... }:

{
  fonts = {
    fonts = with pkgs; [
      scientifica
      material-symbols
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      source-han-sans
      source-han-code-jp
      (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
      cozette
    ];
    fontconfig = {
      antialias = true;
      hinting = {
        enable = true;
        style = "full"; # no difference
        autohint = true; # no difference
      };
      subpixel = {
        rgba = "rgb";
        lcdfilter = "default"; # no difference
      };
    };

    enableDefaultFonts = false;
    # optimizeForVeryHighDPI = lib.mkDefault true;

    fontconfig.defaultFonts = {
      serif = ["Sofia Pro" "Source Han Sans" "Noto Color Emoji"];
      sansSerif = ["Sofia Pro" "Source Han Sans" "Noto Color Emoji"];
      monospace = ["JetBrainsMono Nerd Font" "Source Han Code JP" "Noto Color Emoji"];
      emoji = ["Noto Color Emoji"];
    };
  };
}
