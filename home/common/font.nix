{pkgs, ...}: {
  fontProfiles = {
    enable = true;
    monospace = {
      # family = "Cozette";
      # package = pkgs.cozette;
      family = "JetBrainsMono Nerd Font";
      package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono" "FantasqueSansMono" "Mononoki"];};
    };
    regular = {
      family = "Sofia Pro";
      package = pkgs.useful-fonts;
    };
  };
}
