{
  modulesPath,
  ...
}:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  swapDevices = [
    {
      device = "/persist/swap";
      size = 8192;
    }
  ];
}
