# test.nix in your flake root
# helper nix-instantiate --eval $file
let
  pkgs = import <nixpkgs> { };
  lib = pkgs.lib;
  palettes = import ./home/common/colors/palettes { inherit lib; };
in
builtins.trace (builtins.toJSON palettes) palettes
