{ modulesPath, ... }:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  fileSystems."/local/windows" = {
    device = "/dev/disk/by-uuid/FEC63D8AC63D43E5";
    fsType = "ntfs3";
    options = [
      "rw"
      "uid=1000"
      "gid=100"
      "dmask=022"
      "fmask=133"
      "nofail"
      "x-systemd.automount"
      "x-systemd.device-timeout=5s"
    ];
  };

  fileSystems."/local/nixos/games" = {
    device = "/dev/disk/by-label/NIXOS";
    fsType = "btrfs";
    options = [
      "rw"
      "subvol=@games"
      "compress=zstd"
      "noatime"
    ];
  };

  fileSystems."/local/nixos/dbs" = {
    device = "/dev/disk/by-label/NIXOS";
    fsType = "btrfs";
    options = [
      "rw"
      "subvol=@dbs"
      "compress=zstd"
      "noatime"
    ];
  };

  fileSystems."/local/nixos/data" = {
    device = "/dev/disk/by-label/NIXOS";
    fsType = "btrfs";
    options = [
      "rw"
      "subvol=@data"
      "compress=zstd"
      "noatime"
    ];
  };
  # fileSystems."/mnt/Nixos/Programming" = {
  #   device = "/dev/disk/by-label/NIXOS";
  #   fsType = "btrfs";
  #   options = ["rw" "subvol=Programming" "compress=zstd" "noatime"];
  # };
  fileSystems."/local/storage" = {
    device = "/dev/disk/by-uuid/920881090880EE13";
    fsType = "ntfs3";
    options = [
      "rw"
      "uid=1000"
      "gid=100"
      "dmask=022"
      "fmask=133"
      "nofail"
      "x-systemd.automount"
      "x-systemd.device-timeout=5s"
    ];
  };
}
