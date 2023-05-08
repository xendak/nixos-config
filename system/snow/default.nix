# my main desktop
{ lib, pkgs, inputs, ...}: {
  imports = [
    ../global.nix
    ./btrfs-optin-persistence.nix
    ./hardware-configuration.nix

    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-gpu-amd
    inputs.hardware.nixosModules.common-pc-ssd

    inputs.aagl.nixosModules.default
  ];


  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" "amdgpu" "i2c-dev" "i2c-i801" ];
  boot.loader.systemd-boot.enable = true;
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    # kernelPackages = pkgs.linuxPackages_xanmod_latest;
    supportedFilesystems = [ "btrfs" "ntfs" ];
  };

  # genshin
  programs.an-anime-game-launcher.enable = true;
  programs.the-honkers-railway-launcher.enable = true;

  # NTFS-3G for Windows Partititions
  environment.systemPackages = [
    pkgs.ntfs3g
    pkgs.openrgb-with-all-plugins
    pkgs.i2c-tools
  ];

  services.hardware = {
    openrgb.enable = true;
    openrgb.motherboard = "intel";
  };
  hardware = {
    i2c.enable = true;

    cpu.intel.updateMicrocode = true;
    opengl = {
      enable = true;
      # extraPackages = with pkgs; [ amdvlk ];
      driSupport = true;
      driSupport32Bit = true;
    };
    opentabletdriver.enable = true;
  };

  networking.useDHCP = lib.mkDefault true;

  system.stateVersion = "23.05";

}
