{
  inputs,
  lib,
  pkgs,
  config,
  outputs,
  ...
}: let
  inherit (inputs.nix-colors) colorSchemes;
in {
  imports =
    [
      #inputs.impermanence.nixosModules.home-manager.impermanence
      inputs.nix-colors.homeManagerModule
      ../common/colors
    ]
    ++ (builtins.attrValues outputs.homeManagerModules);

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
    };
  };

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = ["nix-command" "flakes"];
      warn-dirty = false;
    };
  };

  # Something? -------------
  colorscheme = lib.mkDefault colorSchemes.nord; #material-palenight;

  home.file.".colorscheme".text = config.colorscheme.slug;
}
