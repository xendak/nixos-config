{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  fileSystems."/mnt/Windows" = {
    device = "/dev/disk/by-uuid/A45A7C1C5A7BE986";
    fsType = "ntfs3";
    options = ["rw" "uid=1000"];
  };
}
