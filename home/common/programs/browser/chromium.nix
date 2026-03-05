{
  pkgs,
  lib,
  ...
}:
let
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
in
{

  # xdg.mimeApps.defaultApplications = {
  #   "text/html" = ["chromium-browser.desktop"];
  #   "text/xml" = ["chromium-browser.desktop"];
  #   "x-scheme-handler/http" = ["chromium-browser.desktop"];
  #   "x-scheme-handler/https" = ["chromium-browser.desktop"];
  #   "x-scheme-handler/about" = ["chromium-browser.desktop"];
  #   "x-scheme-handler/unknown" = ["chromium-browser.desktop"];
  # };

  programs.chromium = {
    enable = true;
    package = baseChromium;
    # searchEngine = "https://www.google.com/search?q=";
    # extraOpts = {
    #   "BrowserSignin" = 0;
    #   "SyncDisabled" = true;
    #   "PasswordManagerEnabled" = false;
    #   "SpellcheckLanguage" = [
    #     "en-US"
    #     "pt-BR"
    #     "ja-JP"
    #   ];
    #   "ui.zoom.force_enable_zoom_scrollbars" = true;
    # };

    # extensions = [
    #   "cbghhgpcnddeihccjmnadmkaejncjndb"
    #   "jinjaccalgkegednnccohejagnlnfdag"
    #   "dbepggeogbaibhgnhhndojpepiihcmeb"
    #   "nngceckbapebfimnlniiiahkandclblb"
    #   "cjpalhdlnbpafiamejdnhcphjbkeiagm"
    # ];

    extensions =
      let
        createChromiumExtensionFor =
          browserVersion:
          {
            id,
            sha256,
            version,
          }:
          {
            inherit id;
            crxPath = builtins.fetchurl {
              url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
              name = "${id}.crx";
              inherit sha256;
            };
            inherit version;
          };
        createChromiumExtension = createChromiumExtensionFor (lib.versions.major baseChromium.version);
      in
      [
        (createChromiumExtension {
          # Vencord
          id = "cbghhgpcnddeihccjmnadmkaejncjndb";
          sha256 = "sha256:0l7bz754pgcqhs0ijdrd7cizncjvchglaqknqsrg843r2xyddsm1";
          version = "1.11.9";
        })
        (createChromiumExtension {
          # ViolentMonkey
          id = "jinjaccalgkegednnccohejagnlnfdag";
          sha256 = "sha256:0vwgbqkim8s0ai9bj2vhhs590vv022s58d5m6q80zy6p57gxlpih";
          version = "2.31.0";
        })
        (createChromiumExtension {
          # Vimium
          id = "dbepggeogbaibhgnhhndojpepiihcmeb";
          sha256 = "sha256:13a56wx9i4plj8k9ifbi0aq1a9fipvfkm61r2sysbqd9z3nnwsy1";
          version = "2.1.2";
        })
        (createChromiumExtension {
          # Bitwarden
          id = "nngceckbapebfimnlniiiahkandclblb";
          sha256 = "sha256:1fiyyl36rh9nfzsx8pcjmgwdnfpy886aklnhrjb0rvkd7zf3w1d1";
          version = "2025.3.2";
        })
        (createChromiumExtension {
          # Ublock
          id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
          sha256 = "sha256:0ksbby7sim15b6ym8m3yjw3zz0942r9sg43grqpv1cckb55c4ha8";
          version = "1.63.2";
        })
      ];
  };
}
