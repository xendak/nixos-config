{ config, pkgs, ... }:

let
  # File list containing all Chromium paths to sync
  chromiumFilelist = pkgs.writeText "chromium-sync-filelist" ''
    Default/Cookies
    Default/Bookmarks
    Default/Login Data
    Default/History
    Default/Web Data
    Local State
    Default/Preferences
    Default/Local Storage
    Default/Sessions
    Default/Session Storage
  '';

  # Main sync script
  syncScript = pkgs.writeShellScriptBin "sync_browser" ''
    #!/bin/sh
    USER=$(whoami)
    PERSIST="/persist/home/$USER/.config/chromium"
    LIVE="/home/$USER/.config/chromium"
    FILELIST="${chromiumFilelist}"

    # Argument handling
    if [ "$1" = "persist-to-live" ]; then
      SOURCE="$PERSIST"
      DEST="$LIVE"
      OPTS="-a --mkpath"
    elif [ "$1" = "live-to-persist" ]; then
      SOURCE="$LIVE"
      DEST="$PERSIST"
      OPTS="-a --update --mkpath"
    else
      # Default to force sync from LIVE to PERSIST
      SOURCE="$LIVE"
      DEST="$PERSIST"
      OPTS="-a --mkpath"
    fi

    # Create destination directory if needed
    mkdir -p "$(dirname "$DEST")"

    # Perform sync with file list
    rsync $OPTS --files-from="$FILELIST" "$SOURCE/" "$DEST/"
  '';
in {
  # Add sync script to system PATH
  environment.systemPackages = [ syncScript ];

  # Systemd service to restore data on login
  systemd.user.services."chromium-sync-persist-to-live" = {
    Unit = {
      Description = "Restore Chromium data from persistent storage";
      After = [ "network-online.target" "hyprland-session.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${syncScript}/bin/sync_browser persist-to-live";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  # Systemd service and timer for hourly sync
  systemd.user.services."chromium-sync-live-to-persist" = {
    Unit.Description = "Backup Chromium data to persistent storage";
    Service = {
      Type = "oneshot";
      ExecStart = "${syncScript}/bin/sync_browser live-to-persist";
    };
  };

  systemd.user.timers."chromium-sync-live-to-persist" = {
    Unit.Description = "Hourly Chromium data backup timer";
    Timer = {
      OnUnitActiveSec = "1h";
      OnBootSec = "5m";
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
