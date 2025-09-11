{
  inputs,
  lib,
  pkgs,
  config,
  outputs,
  ...
}:
{
  imports = [
    inputs.impermanence.homeManagerModules.impermanence
    inputs.nix-colors.homeManagerModule

    # Set default color
    ./common/colors/gorgoroth.nix
  ]
  ++ (builtins.attrValues outputs.homeManagerModules);

  nixpkgs = lib.mkIf (config.home.username == "nixos") {
    overlays = builtins.attrValues outputs.overlays;
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      warn-dirty = false;
    };
  };

  themes.light = import ./common/colors/grayscale-nier.nix;
  themes.dark = import ./common/colors/luna.nix;
  themes.default = import ./common/colors/gorgoroth.nix;

  home.file.".colorscheme".text = config.colorscheme.slug;
}
