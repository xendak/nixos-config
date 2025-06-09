{ pkgs, ... }:
let
  # not used.
  # logins.json
  zenFileList = pkgs.writeText "zen-sync-filelist" ''
    places.sqlite
    cookies.sqlite
    key4.db
    cert9.db
    permissions.sqlite
    prefs.js
    sessionstore.jsonlz4
    storage
    cache2/entries
    bookmarkbackups
    addons.json
    addonStartup.json.lz4
    extension-preferences.json
    extension-store-menus
    extension-settings.json
    extensions
    extensions.json
    zen-keyboard-shortcuts.json
    zen-themes.json
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
    Default/Extension Rules
    Default/Extensions
    Default/Extension Scripts
    Default/Extension State
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
    if [ "$2" = "persist-to-live" ]; then
      SOURCE="$PERSIST"
      DEST="$LIVE"
      OPTS="-arv --mkpath"
    elif [ "$2" = "live-to-persist" ]; then
      SOURCE="$LIVE"
      DEST="$PERSIST"
      OPTS="-arv --update --mkpath"
      if [ "$1" = "zen" ]; then
        # handle sessionstore not existing when browser is actually opened.
        # If source directory has recovery file but no sessionstore file
        if [ -f "$SOURCE/sessionstore-backups/recovery.jsonlz4" ] && [ ! -f "$DEST/sessionstore.jsonlz4" ]; then
          ${pkgs.rsync}/bin/rsync -av --update "$SOURCE/sessionstore-backups/recovery.jsonlz4" "$DEST/sessionstore.jsonlz4"
        fi
      fi
    else
      # Default to force sync from LIVE to PERSIST
      SOURCE="$LIVE"
      DEST="$PERSIST"
      OPTS="-arv --mkpath"
    fi

    # Make sure the source directory exists before attempting sync
    if [ ! -d "$SOURCE" ]; then
      echo "Source directory $SOURCE does not exist. Skipping sync."
      exit 0
    fi

    # Make sure the destination directory exists
    mkdir -p "$DEST"


    # Perform sync with file list
    ${pkgs.rsync}/bin/rsync $OPTS --files-from="$FILELIST" "$SOURCE/" "$DEST/"
  '';

  allSyncScript = pkgs.writeShellScriptBin "all-sync" ''
    # Run both sync operations in parallel
    if [ $(pgrep "zen") ]; then
      pkill -9 zen
      pkill -9 chromiumo
      sleep 2
    fi

    ${syncScript}/bin/sync-browser chromium $1 &
    ${syncScript}/bin/sync-browser zen $1 &

    # Wait for both to complete
    wait


    # Check exit status
    if [ $? -ne 0 ]; then
      echo "One or more sync operations failed" >&2
      echo "[ERROR]: $(date)" >> /tmp/browser-sync.log
      exit 1
    else
      echo "[SUCCESS]: $(date)" >> /tmp/browser-sync.log
    fi
  '';
in
{
  # Add sync script to system PATH
  environment.systemPackages = [
    syncScript
    allSyncScript
  ];

  # systemd.user.services."hourly-browser-sync-live-to-persist" = {
  #   enable = true;
  #   description = "Moves ephemeral chromium data to persistent storage";
  #   serviceConfig = {
  #     Type = "oneshot";
  #     StandardOutput = "journal";
  #     StandardError = "journal";
  #   };
  #   script = ''
  #     ${allSyncScript}/bin/all-sync live-to-persist
  #   '';
  # };

  # systemd.user.timers."hourly-browser-sync-live-to-persist" = {
  #   description = "Restore Chromium data from persistent storage";
  #   wantedBy = ["timers.target"];
  #   timerConfig = {
  #     OnUnitActiveSec = "1h";
  #     OnBootSec = "5m";
  #     Unit = "hourly-browser-sync-live-to-persist.service";
  #     Persistent = true;
  #   };
  # };

  # Systemd service to restore data on login
  systemd.user.services."browser-sync-login" = {
    enable = true;
    description = "Restore browser data from persistent storage";
    after = [ "hyprland-session.target" ];
    wantedBy = [ "default.target" ];
    serviceConfig = {
      Type = "oneshot";
    };
    script = ''
      ${allSyncScript}/bin/all-sync persist-to-live
    '';
  };

  # Systemd service to restore data on session end
  # systemd.user.services."browser-sync-shutdown" = {
  #   enable = true;
  #   description = "Sync browser data before user session ends";
  #   unitConfig = {
  #     DefaultDependencies = "no";
  #     Before = [ "exit.target" ];
  #   };
  #   wantedBy = [ "exit.target" ];

  #   serviceConfig = {
  #     Type = "oneshot";
  #     TimeoutStopSec = "60s";
  #     StandardOutput = "journal";
  #     StandardError = "journal";

  #     # RemainAfterExit = true;
  #   };
  #   script = ''
  #     ${allSyncScript}/bin/all-sync live-to-persist
  #   '';
  # };
}
