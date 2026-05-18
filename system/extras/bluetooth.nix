{
  pkgs,
  ...
}:
{
  environment.systemPackages = [
    pkgs.rfkill_udev
  ];

  services.blueman.enable = true;

  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
        ControllerMode = "dual";
      };
      Policy = {
        AutoEnable = "false";
      };
    };
  };
}
