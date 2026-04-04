{
  inputs,
  pkgs,
  config,
  ...
}:

let
  mkLockedAttrs = builtins.mapAttrs (
    _: value: {
      Value = value;
      Status = "locked";
    }
  );

  getIcon =
    url: sha256:
    pkgs.fetchurl {
      inherit url sha256;
    };
in
{
  imports = [
    inputs.zen-browser.homeModules.beta
  ];

  home.sessionVariables.MOZ_LEGACY_PROFILES = "1";

  home.persistence."/persist".directories = [
    ".config/zen/${config.home.username}"
  ];

  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "zen-beta.desktop" ];
    "text/xml" = [ "zen-beta.desktop" ];
    "x-scheme-handler/http" = [ "zen-beta.desktop" ];
    "x-scheme-handler/https" = [ "zen-beta.desktop" ];
    "x-scheme-handler/about" = [ "zen-beta.desktop" ];
    "x-scheme-handler/unknown" = [ "zen-beta.desktop" ];
  };

  programs.zen-browser = {
    enable = true;
    package = inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default;

    policies = {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      OfferToSaveLogins = false;
      PasswordManagerEnabled = false;
      DisableFirefoxAccounts = true;
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };
      Preferences = mkLockedAttrs {
        "browser.aboutConfig.showWarning" = false;
        "browser.tabs.warnOnClose" = false;
        "privacy.resistFingerprinting" = true;
        "dom.battery.enabled" = false;
        "network.cookie.cookieBehavior" = 5;
        "browser.newtabpage.activity-stream.feeds.topsites" = false;
      };
    };

    profiles.xendak = {
      path = "xendak";
      isDefault = true;

      settings = {
        # fix theme issues
        "layout.css.prefers-color-scheme.content-override" = 3;
        "zen.view.window.scheme" = 2;
        # enable terminal portal
        "widget.use-xdg-desktop-portal.file-picker" = 1;
        "zen.welcome-screen.seen" = true;
        "zen.urlbar.behavior" = "floating-on-type";
      };

      # :Search Engines
      search = {
        force = true;
        default = "google";
        engines = {
          "nix-pkgs" = {
            name = "Nix Packages";
            urls = [ { template = "https://search.nixos.org/packages?channel=25.11&query={searchTerms}"; } ];
            icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
            definedAliases = [ "np" ];
          };
          "nix-opts" = {
            name = "Nix Options";
            urls = [ { template = "https://search.nixos.org/options?channel=25.11&query={searchTerms}"; } ];
            icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
            definedAliases = [ "no" ];
          };
          "hm-opts" = {
            name = "Home Manager";
            urls = [
              { template = "https://home-manager-options.extranix.com/?query={searchTerms}&release=master"; }
            ];
            icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
            definedAliases = [ "hm" ];
          };
          "gmaps" = {
            name = "Google Maps";
            urls = [ { template = "https://www.google.com/maps/search/{searchTerms}"; } ];
            definedAliases = [ "mp" ];
            icon = getIcon "https://www.svgrepo.com/show/452221/google-maps.svg" "sha256-Oo40W2RCHluP8Fbhdf7ver9HuvLFtnJ7InNn5zH8Ufg=";
          };
          "zig-std" = {
            name = "Zig Standard Library";
            urls = [ { template = "https://ziglang.org/documentation/master/std/#?q={searchTerms}"; } ];
            icon = getIcon "https://codeberg.org/ziglang/logo/raw/branch/master/zig-mark.svg" "sha256-7MCjT6X3U+W7FzD5feoze23ycitZqlJpbqkOLqbkXtQ=";
            definedAliases = [ "z" ];
          };
          "google" = {
            name = "Google";
            urls = [ { template = "https://www.google.com/search?q={searchTerms}"; } ];
            definedAliases = [ "g" ];
          };
          "ddg" = {
            name = "DuckDuckGo";
            urls = [ { template = "https://duckduckgo.com/?q={searchTerms}"; } ];
            definedAliases = [ "d" ];
          };
          "perplexity".metaData.hidden = true;
          "bing".metaData.hidden = true;
          "ebay".metaData.hidden = true;
        };
      };

      # :Workspaces
      spacesForce = true;
      spaces = {
        "Default" = {
          id = "de710000-0000-4000-a000-000000000000";
          icon = "󰐱";
          position = 1000;
        };
        "Language" = {
          id = "1a1a0000-0000-4000-a000-000000000000";
          icon = "";
          position = 2000;
        };
        "Study" = {
          id = "574d1000-0000-4000-a000-000000000000";
          icon = "󰑴";
          position = 3000;
        };
      };

      pinsForce = true;
      pins =
        let
          spaces = config.programs.zen-browser.profiles.xendak.spaces;
        in
        {
          # :Essentials
          "Discord" = {
            id = "3c6d482a-8c9e-4a6b-9f2d-1a2b3c4d5e6f";
            url = "https://discord.com/channels/@me/1078812284006174851";
            isEssential = true;
            position = 1;
          };
          "Monkeytype" = {
            id = "a1b2c3d4-e5f6-4a5b-9c8d-7e6f5a4b3c2d";
            url = "https://monkeytype.com";
            isEssential = true;
            position = 2;
          };
          "GitHub" = {
            id = "f6e5d4c3-b2a1-4098-8765-43210fedcba9";
            url = "https://github.com/xendak";
            isEssential = true;
            position = 3;
          };

          # :Default
          "Gemini" = {
            id = "bcde1234-5678-4901-a234-567890abcdef";
            url = "https://gemini.google.com/app";
            workspace = spaces."Default".id;
            position = 10;
          };
          "Claude" = {
            id = "cdef2345-6789-4012-b345-67890abcdef1";
            url = "https://claude.ai/chat";
            workspace = spaces."Default".id;
            position = 11;
          };

          # :Languages
          "Gemini-Lang" = {
            id = "defa3456-7890-4123-c456-7890abcdef12";
            url = "https://gemini.google.com/app";
            workspace = spaces."Language".id;
            position = 10;
          };
          "Claude-Lang" = {
            id = "efab4567-8901-4234-d567-890abcdef123";
            url = "https://claude.ai/chat";
            workspace = spaces."Language".id;
            position = 11;
          };
          "Translate" = {
            id = "fabc5678-9012-4345-e678-90abcdef1234";
            url = "https://translate.google.com/?sl=ja&tl=en";
            workspace = spaces."Language".id;
            position = 20;
          };
          "DeepL" = {
            id = "abcd6789-0123-4456-f789-0abcdef12345";
            url = "https://www.deepl.com/translator#ja/en/";
            workspace = spaces."Language".id;
            position = 21;
          };

          # :Study
          "Gemini-Study" = {
            id = "bcde7890-1234-4567-a890-1abcdef12345";
            url = "https://gemini.google.com/app";
            workspace = spaces."Study".id;
            position = 10;
          };
          "Claude-Study" = {
            id = "cdef8901-2345-4678-b901-2abcdef12345";
            url = "https://claude.ai/chat";
            workspace = spaces."Study".id;
            position = 11;
          };
          "Canvas" = {
            id = "defa9012-3456-4789-c012-3abcdef12345";
            url = "https://canvas.pucminas.br/";
            workspace = spaces."Study".id;
            position = 20;
          };
        };

      # :Extensions
      extensions.packages = with inputs.firefox-addons.packages.${pkgs.stdenv.hostPlatform.system}; [
        vimium
        ublock-origin
        bitwarden
        violentmonkey
        darkreader
      ];
    };
  };
}
