{
  inputs,
  lib,
  pkgs,
  config,
  outputs,
  ...
}:
let
  inherit (inputs.nix-colors) colorSchemes;
in
{
  imports = [
    inputs.impermanence.homeManagerModules.impermanence
    inputs.nix-colors.homeManagerModule

    # ../common/colors
    ../common/colors/gorgoroth.nix
    # ../common/colors/grayscale-nier.nix

  ]
  ++ (builtins.attrValues outputs.homeManagerModules);

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

  # Something? -------------
  # colorscheme = lib.mkDefault colorSchemes.nord; # material-palenight;
  themes.light = import ../common/colors/grayscale-nier.nix;
  themes.dark = import ../common/colors/luna.nix;
  themes.default = import ../common/colors/gorgoroth.nix;

  home.file.".colorscheme".text = config.colorscheme.slug;
}
