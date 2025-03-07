{
  pkgs,
  lib,
  config,
  ...
}:
let
  # Persistence targets
  chromiumFiles = [
    "Default/Cookies"
    "Default/Bookmarks"
    "Default/Login Data"
    "Default/History"
    "Default/Web Data"
    "Local State"
    "Default/Preferences"
  ];
  
  chromiumDirs = [
    "Default/Local Storage"
    "Default/Sessions"
    "Default/Session Storage"
  ];

  # Common command line arguments
  commandLineArgs = [
    "--disable-sync"
    "--no-default-browser-check"
    "--force-dark-mode"
    "--ozone-platform=wayland"
    "--enable-features=UseOzonePlatform"
    "--enable-features=BlockThirdPartyCookiesInIncognito"
    "--no-service-autorun"
    "--disable-features=PreloadMediaEngagementData,MediaEngagementBypassAutoplayPolicies"
    "--disable-reading-from-canvas"
    "--no-pings"
    "--no-first-run"
    "--no-experiments"
    "--no-crash-upload"
    "--disable-wake-on-wifi"
    "--enable-features=VaapiVideoDecodeLinuxGL"
    "--ignore-gpu-blocklist"
    "--enable-zero-copy"
    "--disable-breakpad"
    "--disable-sync"
    "--disable-speech-api"
    "--disable-speech-synthesis-api"
  ];

  # Base Chromium package
  baseChromium = pkgs.ungoogled-chromium.override {
    enableWideVine = true;
    inherit commandLineArgs;
  };

  # Wrapper script using Nix-defined files/dirs
  persistenceWrapper = pkgs.writeShellScriptBin "chromium-wrapper" ''
    #!/bin/sh
    set -euo pipefail

    USER="$(whoami)"
    PERSIST_ROOT="/persist/home/$USER/.config/chromium"
    LIVE_ROOT="/home/$USER/.config/chromium"

    # Create directory structure
    mkdir -p "$LIVE_ROOT/Default" "$PERSIST_ROOT/Default"

    # Sync from persist to live
    ${pkgs.rsync}/bin/rsync -av \
      --include='/Default' \
      ${lib.concatMapStrings (d: "--include='${d}/***' ") chromiumDirs} \
      ${lib.concatMapStrings (f: "--include='${f}' ") chromiumFiles} \
      --exclude='*' \
      "$PERSIST_ROOT/" "$LIVE_ROOT/"

    # Launch Chromium

    # Sync back to persist
    cleanup() {
      ${pkgs.rsync}/bin/rsync -av \
        --include='/Default' \
        ${lib.concatMapStrings (d: "--include='${d}/***' ") chromiumDirs} \
        ${lib.concatMapStrings (f: "--include='${f}' ") chromiumFiles} \
        --exclude='*' \
        "$LIVE_ROOT/" "$PERSIST_ROOT/"
    }
    trap cleanup EXIT

    exec "${baseChromium}/bin/chromium" ${lib.escapeShellArgs commandLineArgs} "$@"
  '';

  # Final wrapped package
  wrappedChromium = pkgs.symlinkJoin {
    name = "chromium";
    paths = [ baseChromium ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/chromium \
        --run "exec ${persistenceWrapper}/bin/chromium-wrapper"
    '';
  };
in {
  # Rest of your configuration remains the same
  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "chromium-browser.desktop" ];
    "text/xml" = [ "chromium-browser.desktop" ];
    "x-scheme-handler/http" = [ "chromium-browser.desktop" ];
    "x-scheme-handler/https" = [ "chromium-browser.desktop" ];
    "x-scheme-handler/about" = [ "chromium-browser.desktop" ];
    "x-scheme-handler/unknown" = [ "chromium-browser.desktop" ];
  };

  programs.chromium = {
    enable = true;
    package = wrappedChromium;
    # searchEngine = "https://www.google.com/search?q=";
    # extraOpts = {
    #   "BrowserSignin" = 0;
    #   "SyncDisabled" = true;
    #   "PasswordManagerEnabled" = false;
    #   "SpellcheckLanguage" = [ "en-US" "pt-BR" "ja-JP" ];
    #   "ui.zoom.force_enable_zoom_scrollbars" = true;
    # };

    extensions = let
      createChromiumExtensionFor = browserVersion: {
        id,
        sha256,
        version,
      }: {
        inherit id;
        crxPath = builtins.fetchurl {
          url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
          name = "${id}.crx";
          inherit sha256;
        };
        inherit version;
      };
      createChromiumExtension = createChromiumExtensionFor (lib.versions.major baseChromium.version);
    in [
      (createChromiumExtension { # Vimium
        id = "dbepggeogbaibhgnhhndojpepiihcmeb";
        sha256 = "sha256:0m8xski05w2r8igj675sxrlkzxlrl59j3a7m0r6c8pwcvka0r88d";
        version = "2.1.2";
      })
      (createChromiumExtension { # Bitwarden
        id = "nngceckbapebfimnlniiiahkandclblb";
        sha256 = "sha256:14mk4x3nggkggf68a3bafry9vk54yxcxlsczzs4qmp7m03y16a1n";
        version = "2025.2.1";
      })
      (createChromiumExtension { # Ublock
        id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
        sha256 = "sha256:01kk94l38qqp2rbyylswjs8q25kcjaqvvh5b8088xria5mbrhskl";
        version = "1.62.0";
      })
    ];
  };
}
