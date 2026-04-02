{ lib, ... }:
{
  hardware.bluetooth = {
    enable = true;
    settings.General.Enable = "Source,Sink,Media,Socket";
  };
  services.blueman.enable = true;

  environment.etc."/bluetooth/main.conf".text = lib.mkDefault ''
    [General]
    ControllerMode=dual
    Enable=Source,Sink,Media,Socket

    [Policy]
    AutoEnable=false
  '';
}
