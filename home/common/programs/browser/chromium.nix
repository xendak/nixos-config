{
  pkgs,
  lib,
  config,
  ...
}: let
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
in {
  # Rest of your configuration remains the same
  xdg.mimeApps.defaultApplications = {
    "text/html" = ["chromium-browser.desktop"];
    "text/xml" = ["chromium-browser.desktop"];
    "x-scheme-handler/http" = ["chromium-browser.desktop"];
    "x-scheme-handler/https" = ["chromium-browser.desktop"];
    "x-scheme-handler/about" = ["chromium-browser.desktop"];
    "x-scheme-handler/unknown" = ["chromium-browser.desktop"];
  };

  home.sessionVariables = {
    BROWSER = "chromium";
  };

  programs.chromium = {
    enable = true;
    package = baseChromium;
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
      (createChromiumExtension {
        # Vencord
        id = "cbghhgpcnddeihccjmnadmkaejncjndb";
        sha256 = "sha256:0z5mi01n47wvrl5g6x0h0y90v7jjrs8ymdlzlx9rpwd52r6sh7wb";
        version = "1.11.7";
      })
      (createChromiumExtension {
        # Vimium
        id = "dbepggeogbaibhgnhhndojpepiihcmeb";
        sha256 = "sha256:0m8xski05w2r8igj675sxrlkzxlrl59j3a7m0r6c8pwcvka0r88d";
        version = "2.1.2";
      })
      (createChromiumExtension {
        # Bitwarden
        id = "nngceckbapebfimnlniiiahkandclblb";
        sha256 = "sha256:1cqxid070yiqhw99najwg3nj3csdr8cp12pd0gggnfp34c8n2dig";
        version = "2025.2.1";
      })
      (createChromiumExtension {
        # Ublock
        id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
        sha256 = "sha256:1lnk0k8zy0w33cxpv93q1am0d7ds2na64zshvbwdnbjq8x4sw5p6";
        version = "1.62.0";
      })
    ];
  };
}
