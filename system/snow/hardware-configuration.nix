{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems."/mnt/Windows" =
    { device = "/dev/disk/by-uuid/FEC63D8AC63D43E5";
      fsType = "ntfs3";
      options = [ "rw" "uid=1000" ];
    };
  fileSystems."/mnt/LocalDisk" =
    { device = "/dev/disk/by-uuid/3EA83264A8321ABB";
      fsType = "ntfs3";
      options = [ "rw" "uid=1000" ];
    };
  fileSystems."/mnt/Storage" =
    { device = "/dev/disk/by-uuid/920881090880EE13";
      fsType = "ntfs3";
      options = [ "rw" "uid=1000" ];
    };
}

