{ lib, ... }:
let
  paletteFiles = builtins.attrNames (builtins.readDir ./.);
  nixFiles = lib.filter (f: lib.hasSuffix ".nix" f && f != "default.nix") paletteFiles;

  toAttrSet =
    file:
    let
      name = lib.removeSuffix ".nix" file;
      value = import ./${file};
    in
    {
      inherit name value;
    };

in
lib.listToAttrs (map toAttrSet nixFiles)
