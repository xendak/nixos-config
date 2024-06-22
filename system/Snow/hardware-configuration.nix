{
  modulesPath,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  fileSystems."/mnt/Windows" = {
    device = "/dev/disk/by-uuid/FEC63D8AC63D43E5";
    fsType = "ntfs3";
    options = ["rw" "uid=1000"];
  };
  fileSystems."/mnt/LocalDisk" = {
    device = "/dev/disk/by-uuid/50A43AA6A43A8F0A";
    fsType = "ntfs3";
    options = ["rw" "uid=1000"];
  };
  fileSystems."/mnt/Nixos" = {
    device = "/dev/disk/by-label/NIXOS";
    fsType = "btrfs";
    options = ["rw" "compress=zstd" "noatime" "uid=1000"];
  };
  # fileSystems."/mnt/Nixos/Programming" = {
  #   device = "/dev/disk/by-label/NIXOS";
  #   fsType = "btrfs";
  #   options = ["rw" "subvol=Programming" "compress=zstd" "noatime"];
  # };
  fileSystems."/mnt/Storage" = {
    device = "/dev/disk/by-uuid/920881090880EE13";
    fsType = "ntfs3";
    options = ["rw" "uid=1000"];
  };
}
