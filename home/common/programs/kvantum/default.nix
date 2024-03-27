{ config, pkgs, lib, ... }:
{
  imports = [
    ./kvantum-svg.nix
    ./kvantum-kvconfig.nix
  ];

  # home.file = {
  #   ".config/Kvantum/kvantum.kvconfig" ={
  #     source = config.lib.file.mkOutOfStoreSymlink "/persist/home/${config.home.username}/Flake/home/${config.home.username}/common/programs/kvantum/config";
  #   };
  # };

  home.activation = {
    removeExistingKvantumConfig = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
      rm -rf "/home/${config.home.username}/.config/Kvantum/kvantum.kvconfig"
      mkdir -p "/home/${config.home.username}/.config/Kvantum"
    '';

    copyKvantumConfig = let
      newKvantumConfig = pkgs.writeText "tmp_kvconfig" (builtins.readFile ./config);
    in lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      rm -rf "/home/${config.home.username}/.config/Kvantum/kvantum.kvconfig"
      mkdir -p "/home/${config.home.username}/.config/Kvantum"
      cp "${newKvantumConfig}" "/home/${config.home.username}/.config/Kvantum/kvantum.kvconfig"
      chmod 666 "/home/${config.home.username}/.config/Kvantum/kvantum.kvconfig"
    '';
  };
}
