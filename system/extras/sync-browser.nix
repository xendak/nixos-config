{pkgs, ...}: let
  zenFileList = pkgs.writeText "zen-sync-filelist" ''
    "places.sqlite"
    "cookies.sqlite"
    "logins.json"
    "key4.db"
    "cert9.db"
    "permissions.sqlite"
    "prefs.js"
    "sessionstore.jsonlz4"
    "storage"
    "cache2/entries"
    "bookmarkbackups"
    "extension-data"
  '';

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
  syncScript = pkgs.writeShellScriptBin "sync-browser" ''
    USER=$(whoami)
    NEUTRAL_PERSIST="/persist/home/$USER/.config"
    NEUTRAL_LIVE="/home/$USER/.config"

    case "$1" in
      chromium)
          PERSIST="$NEUTRAL_PERSIST/chromium"
          LIVE="$NEUTRAL_LIVE/chromium"
          FILELIST="${chromiumFilelist}"
        ;;
        zen)
          PERSIST="$NEUTRAL_PERSIST/zen/$USER"
          LIVE="$NEUTRAL_LIVE/zen/$USER"
          FILELIST="${zenFileList}"
        ;;
        *)
          echo "unsupported browser at the moment"
          exit 1
        ;;
    esac

    # Argument handling
    if [ "$1" = "persist-to-live" ]; then
      SOURCE="$PERSIST"
      DEST="$LIVE"
      OPTS="-arv --mkpath"
    elif [ "$1" = "live-to-persist" ]; then
      SOURCE="$LIVE"
      DEST="$PERSIST"
      OPTS="-arv --update --mkpath"
    else
      # Default to force sync from LIVE to PERSIST
      SOURCE="$LIVE"
      DEST="$PERSIST"
      OPTS="-arv --mkpath"
    fi

    # Perform sync with file list
    rsync $OPTS --files-from="$FILELIST" "$SOURCE/" "$DEST/"
  '';

  allSyncScript = pkgs.writeShellScriptBin "all-sync" ''
    # Run both sync operations in parallel
    ${syncScript}/bin/sync-browser chromium $1 &
    ${syncScript}/bin/sync-browser zen $1 &

    # Wait for both to complete
    wait

    # Check exit status
    if [ $? -ne 0 ]; then
      echo "One or more sync operations failed" >&2
      exit 1
    fi
  '';
in {
  # Add sync script to system PATH
  environment.systemPackages = [
    syncScript
    allSyncScript
  ];

  systemd.user.services."chromium-sync-live-to-persist" = {
    enable = true;
    description = "Moves ephemeral chromium data to persistent storage";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "sh ${syncScript}/bin/sync-browser chromium live-to-persist";
    };
  };

  systemd.user.timers."chromium-sync-live-to-persist" = {
    description = "Restore Chromium data from persistent storage";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnUnitActiveSec = "1h";
      OnBootSec = "5m";
      Unit = "chromium-sync-live-to-persist.service";
    };
  };

  # Systemd service to restore data on login
  systemd.user.services."browser-sync-login" = {
    enable = true;
    description = "Restore browser data from persistent storage";
    after = ["hyprland-session.target"];
    wantedBy = ["default.target"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "sh ${allSyncScript}/bin/all-sync persist-to-live";
    };
  };

  # Systemd service to restore data on shutdown
  systemd.services."browser-sync-shutdown" = {
    enable = true;
    description = "Sync browser data before shutdown";
    unitConfig = {
      # Equivalent to defaultDependencies = false in NixOS
      DefaultDependencies = "no";
      Before = ["shutdown.target" "halt.target" "poweroff.target" "reboot.target"];
      Conflicts = ["reboot.target" "halt.target" "poweroff.target"];
    };
    wantedBy = ["shutdown.target"];

    serviceConfig = {
      Type = "oneshot";
      ExecStop = "sh ${allSyncScript}/bin/all-sync live-to-persist";
      TimeoutStopSec = "60s";
      RemainAfterExit = true;
    };
  };
}
