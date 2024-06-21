{
  pkgs,
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
  home.package = with pkgs; [
    nyxt
  ];
  # Add ungoogled-chromium with extensions
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium.override {
      commandLineArgs = [
        "--disable-sync"
        "--no-default-browser-check"
        "--force-dark-mode"
        "--ozone-platform=wayland"
        "--enable-features=UseOzonePlatform"
        "--enable-features=BlockThirdPartyCookiesInIncognito"
        "--gtk-version=4"

        "--no-service-autorun"
        "--disable-features=PreloadMediaEngagementData,MediaEngagementBypassAutoplayPolicies"
        "--disable-reading-from-canvas"
        "--no-pings"
        "--no-first-run"
        "--no-experiments"
        "--no-crash-upload"
        "--disable-wake-on-wifi"
        "--disable-breakpad"
        "--disable-sync"
        "--disable-speech-api"
        "--disable-speech-synthesis-api"
      ];
    };
    extensions = [
      {id = "mnjggcdmjocbbbhaepdhchncahnbgone";} # sponsor block
      {id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";} # ublock
      {id = "nngceckbapebfimnlniiiahkandclblb";} # bitwarden
      {id = "dbepggeogbaibhgnhhndojpepiihcmeb";} # tab manager
    ];

    # for now i will keep using the default pass manager

    #searchEngine = "https://www.google.com/search?q=";
    #extraOpts = {
    #  "BrowserSignin" = 0;
    #  "SyncDisabled" = true;
    #  "PasswordManagerEnabled" = false;
    #  "SpellcheckLanguage" = [ "pt-BR" "en-US" ];
    #  "ui.zoom.force_enable_zoom_scrollbars" = true;
    #};
    # extensions = let
    #   createChromiumExtensionFor = browserVersion: {
    #     id,
    #     sha256,
    #     version,
    #   }: {
    #     inherit id;
    #     crxPath = builtins.fetchurl {
    #       url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
    #       name = "${id}.crx";
    #       inherit sha256;
    #     };
    #     inherit version;
    #   };
    #   createChromiumExtension = createChromiumExtensionFor (lib.versions.major pkgs.ungoogled-chromium.version);
    # in [
    #   (createChromiumExtension {
    #     # Vimium
    #     id = "dbepggeogbaibhgnhhndojpepiihcmeb";
    #     sha256 = "sha256:0kc4dzpydfvbdr30jl5p02cx9y3k4283rgj5p6dqbrly02j3bamz";
    #     version = "1.67.4";
    #   })
    #   (createChromiumExtension {
    #     # ViolentMonkey
    #     id = "jinjaccalgkegednnccohejagnlnfdag";
    #     sha256 = "sha256:0klm9rqd4bwcjp3azn9bca4zwd3gz1mpqsrn8gz16k2shp7p5yh8";
    #     version = "2.14.0";
    #   })
    #   (createChromiumExtension {
    #     # Bitwarden
    #     id = "nngceckbapebfimnlniiiahkandclblb";
    #     sha256 = "sha256:0z2v79gny5i6mi6d9axn5saf1fzgbjci4r5ykqa6wwjmgbi29gyw";
    #     version = "2024.6.2";
    #   })
    #   (createChromiumExtension {
    #     # uBlock Origin
    #     id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
    #     sha256 = "sha256:0zw3h0vmgsf5bcn38j98284bqswrgw0r8vnpsqzfikyrmb06jp43";
    #     version = "1.47.4";
    #   })
    # ];
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
