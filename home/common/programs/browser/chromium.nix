{
  pkgs,
  lib,
  config,
  ...
}: {
  # browser XDG-OPEN
  # 	xdg.mimeApps.defaultApplications = {
  # 		"text/html" = [ "chromium-browser.desktop" ];
  # 		"text/xml" = [ "chromium-browser.desktop" ];
  # 		"x-scheme-handler/http" = [ "chromium-browser.desktop" ];
  # 		"x-scheme-handler/https" = [ "chromium-browser.desktop" ];
  # 		"x-scheme-handler/about" = [ "chromium-browser.desktop" ];
  # 		"x-scheme-handler/unknown" = [ "chromium-browser.desktop" ];
  # 	};

  # try nyxt
  home.packages = with pkgs; [
    nyxt
  ];
  # Add ungoogled-chromium with extensions
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium.override {
      enableWideVine = true;
      commandLineArgs = [
        "--disable-sync"
        "--no-default-browser-check"
        "--force-dark-mode"
        "--ozone-platform=wayland"
        "--enable-features=UseOzonePlatform"
        "--enable-features=BlockThirdPartyCookiesInIncognito"
        # "--gtk-version=4"

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
    };
    # extensions = [
    #   {id = "mnjggcdmjocbbbhaepdhchncahnbgone";} # sponsor block
    #   {id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";} # ublock
    #   {id = "nngceckbapebfimnlniiiahkandclblb";} # bitwarden
    #   {id = "dbepggeogbaibhgnhhndojpepiihcmeb";} # tab manager
    # ];

    # for now i will keep using the default pass manager

    #searchEngine = "https://www.google.com/search?q=";
    #extraOpts = {
    #  "BrowserSignin" = 0;
    #  "SyncDisabled" = true;
    #  "PasswordManagerEnabled" = false;
    #  "SpellcheckLanguage" = [ "pt-BR" "en-US" ];
    #  "ui.zoom.force_enable_zoom_scrollbars" = true;
    #};
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
      createChromiumExtension = createChromiumExtensionFor (lib.versions.major pkgs.ungoogled-chromium.version);
    in [
      (createChromiumExtension {
        # Vimium
        id = "dbepggeogbaibhgnhhndojpepiihcmeb";
        sha256 = "sha256:0m8xski05w2r8igj675sxrlkzxlrl59j3a7m0r6c8pwcvka0r88d";
        version = "2.1.2";
      })
      # (createChromiumExtension {
      #   # ViolentMonkey
      #   id = "jinjaccalgkegednnccohejagnlnfdag";
      #   sha256 = "sha256:0klm9rqd4bwcjp3azn9bca4zwd3gz1mpqsrn8gz16k2shp7p5yh8";
      #   version = "2.14.0";
      # })
      (createChromiumExtension {
        # Bitwarden
        id = "nngceckbapebfimnlniiiahkandclblb";
        sha256 = "sha256:14mk4x3nggkggf68a3bafry9vk54yxcxlsczzs4qmp7m03y16a1n";
        version = "2024.6.2";
      })
      (createChromiumExtension {
        # uBlock Origin
        id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
        sha256 = "sha256:01kk94l38qqp2rbyylswjs8q25kcjaqvvh5b8088xria5mbrhskl";
        version = "1.58.0";
      })
    ];
  };
  home.persistence = {
    "/persist/home/${config.home.username}" = {
      allowOther = true;
      directories = [
        ".config/chromium/Default/Local Storage"
        # not sure if i'll need this, keep track of.
        #".config/chromium/Default/IndexedDB"
        # ".config/chromium/Default/Certificate"
      ];
      files = [
        ".config/chromium/Default/Cookies"
        ".config/chromium/Default/Bookmarks"
        ".config/chromium/Default/Login Data"
        ".config/chromium/Default/History"
        ".config/chromium/Default/Web Data"
        ".config/chromium/Local State"
        ".config/chromium/Default/Preferences" # doesnt seem to be workng?
      ];
    };
  };
}
