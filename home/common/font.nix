{ pkgs, ... }: 
{
  fontProfiles = {
    enable = true;
    monospace = {
      family = "JetBrainsMono Nerd Font";
      package = (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" "FantasqueSansMono" "Mononoki" ]; });
    };
    regular = {
      family = "Sofia Pro";
      package = pkgs.sofia-pro-font;
    };
  };
}
