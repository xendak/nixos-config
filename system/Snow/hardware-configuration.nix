{ modulesPath, ... }:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems."/mnt/Windows" = {
    device = "/dev/disk/by-uuid/FEC63D8AC63D43E5";
    fsType = "ntfs3";
    options = [
      "rw"
      "uid=1000"
      "gid=100"
      "dmask=022"
      "fmask=133"
      "noauto"
      "x-systemd.automount"
      "x-systemd.device-timeout=5s"
    ];
  };

  # trying to see if this works.
  # dont think i need this anymore.
  # fileSystems."/boot/efi-windows" = {
  #   device = "/dev/nvme0n1p1";
  #   fsType = "vfat";
  #   options = ["ro"];
  # };

  fileSystems."/mnt/LocalDisk" = {
    device = "/dev/disk/by-uuid/50A43AA6A43A8F0A";
    fsType = "ntfs3";
    options = [
      "rw"
      "uid=1000"
      "gid=100"
      "dmask=022"
      "fmask=133"
      "noauto"
      "x-systemd.automount"
      "x-systemd.device-timeout=5s"
    ];
  };

  fileSystems."/mnt/Nixos" = {
    device = "/dev/disk/by-label/NIXOS";
    fsType = "btrfs";
    options = [
      "rw"
      "compress=zstd"
      "noatime"
    ];
  };
  # fileSystems."/mnt/Nixos/Programming" = {
  #   device = "/dev/disk/by-label/NIXOS";
  #   fsType = "btrfs";
  #   options = ["rw" "subvol=Programming" "compress=zstd" "noatime"];
  # };
  fileSystems."/mnt/Storage" = {
    device = "/dev/disk/by-uuid/920881090880EE13";
    fsType = "ntfs3";
    options = [
      "rw"
      "uid=1000"
      "gid=100"
      "dmask=022"
      "fmask=133"
      "noauto"
      "x-systemd.automount"
      "x-systemd.device-timeout=5s"
    ];
  };
}
