{
  pkgs,
  lib,
  inputs,
  ...
}:
let
  zenProfileName = "drops"; # Or your specific profile name
  zenFiles = [
    "places.sqlite"
    "cookies.sqlite"
    "logins.json"
    "key4.db"
    "cert9.db"
    "permissions.sqlite"
    "prefs.js"
    "sessionstore.jsonlz4"
  ];

  zenDirs = [
    "storage"
    "cache2/entries"
    "bookmarkbackups"
    "extension-data"
  ];

  # Wrapper script for Zen
  zenWrapper = pkgs.writeShellScriptBin "zen-wrapper" ''
    #!/bin/bash
    # set -x
    # set -euo pipefail

    USER="$(whoami)"
    PERSIST_ROOT="/persist/home/$USER/.config/zen/${zenProfileName}"
    LIVE_ROOT="/home/$USER/.config/zen/${zenProfileName}"

    mkdir -p "$LIVE_ROOT" "$PERSIST_ROOT"

    # Build include arguments
    INCLUDE_ARGS=()
    ${lib.concatMapStrings (d: ''
    INCLUDE_ARGS+=("--include=${d}/")
    INCLUDE_ARGS+=("--include=${d}/***")
    '') zenDirs}

    ${lib.concatMapStrings (f: ''
    INCLUDE_ARGS+=("--include=${f}")
    '') zenFiles}

    # Sync from persist
    ${pkgs.rsync}/bin/rsync -av \
    --include='/' \
    "''${INCLUDE_ARGS[@]}" \
    --exclude='*' \
    "$PERSIST_ROOT/" "$LIVE_ROOT/"

    cleanup() {
      echo "Syncing Zen data back to persist"
      ${pkgs.rsync}/bin/rsync -av \
      --include='/' \
      "''${INCLUDE_ARGS[@]}" \
      --exclude='*' \
      "$LIVE_ROOT/" "$PERSIST_ROOT/"
    }

    trap cleanup EXIT

    exec "${pkgs.zen-browser}/bin/zen" "$@"
    '';

    wrappedZen = pkgs.symlinkJoin {
      name = "zen-browser";
      paths = [ inputs.zen-browser.packages.${pkgs.system}.default ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
      wrapProgram $out/bin/zen \
      --run "exec ${zenWrapper}/bin/zen-wrapper"
      '';
    };
in {
  # Add similar XDG config if needed
  home.programs = [
    wrappedZen
  ];
}
