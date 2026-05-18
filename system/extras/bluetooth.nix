{
  lib,
  host,
  pkgs,
  ...
}:
let
  resetBluetooth = pkgs.writeShellScriptBin "reset-bluetooth" ''
    set -euo pipefail

    ID_VENDOR="8087"
    ID_PRODUCT="0a2a"

    if [ "$EUID" -ne 0 ]; then
      echo "Please run as root (sudo reset-bluetooth)"
      exit 1
    fi

    echo "Stopping Bluetooth service..."
    systemctl stop bluetooth
    sleep 2

    paths=()
    for p in /sys/bus/usb/devices/*; do 
      if [ -f "$p/idVendor" ] && [ -f "$p/idProduct" ]; then
        found_vnd="$(cat "$p/idVendor" 2>/dev/null)" || true
        found_prd="$(cat "$p/idProduct" 2>/dev/null)" || true

        if [[ "$found_vnd" == "$ID_VENDOR" && "$found_prd" == "$ID_PRODUCT" ]]; then
          paths+=("$p")
        fi
      fi
    done

    if [ ''${#paths[@]} -eq 0 ]; then
      echo "Could not find Bluetooth device $ID_VENDOR:$ID_PRODUCT"
      exit 1
    fi

    echo "Resetting USB device..."
    for p in "''${paths[@]}"; do
      echo 0 > "$p"/authorized
      sleep 1
      echo 1 > "$p"/authorized
    done

    sleep 2

    echo "Starting Bluetooth service..."
    systemctl start bluetooth
    echo "Done! Bluetooth hardware has been reset."
  '';
in
{
  environment.systemPackages = [
    resetBluetooth
    pkgs.rfkill
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
  services.udev.extraRules = lib.mkIf (host == "Snow") ''
    ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="8087", ATTR{idProduct}=="0a2a", ATTR{power/autosuspend}="-1"
  '';
}
