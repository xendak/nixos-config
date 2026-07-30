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

    # FIX(regression): bluez
    package = pkgs.bluez.overrideAttrs (
      finalAttrs: prevAttrs: {
        patches = prevAttrs.patches ++ [
          (pkgs.fetchpatch2 {
            name = "fix-a2dp-sink-priority-regression";
            url = "https://git.kernel.org/pub/scm/bluetooth/bluez.git/patch/?id=066a164a524e4983b850f5659b921cb42f84a0e0";
            hash = "sha256-7n/1Dtoaz0Ll3JL6kDjJGAmu5t04vgYELNMeugHe0Vg=";
          })
        ];
      }
    );
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
