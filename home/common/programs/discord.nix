{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config.colorscheme) colors;

  cfg_file = pkgs.writeText "settings.json" ''
    {
      "SKIP_HOST_UPDATE": true,
      "openasar": {
        "setup": true,
        "quickstart": true,
"css": @import url('https://refact0r.github.io/midnight-discord/midnight.css');:root {--spacing: 12px;--roundness: 16px;--font: 'Sofia Pro';--online-indicator: var(--accent-2);--moon-icon: block; --discord-icon: none;--accent-1: #${colors.base0D};--accent-2: #${colors.base08};--accent-3: #${colors.base01};--accent-4: #${colors.base02};--accent-5: #${colors.base00};--mention: #${colors.base05}60;--mention-hover: #${colors.base05}90;--text-1: #${colors.base07};--text-2: #${colors.base0B};--text-3: #${colors.base05};--text-4: #${colors.base0E};--text-5: #${colors.base03};--bg-1: #${colors.base02};--bg-2: #${colors.base01};--bg-3:  #${colors.base01};--bg-4: #${colors.base00};--hover: #${colors.base0C}60;--active: #${colors.base0B};--message-hover: #${colors.base02}20;}
        "quickstart": true
      }
    }
  '';
in {
  home.packages = [
    (pkgs.discord-canary.override {
      nss = pkgs.nss_latest;
      withOpenASAR = true;
    })
  ];

  home.persistence = {
    "/persist/home/${config.home.username}" = {
      directories = [".config/discordcanary"];
      allowOther = true;
    };
  };

  home.activation = {
    removeExistingDiscordConfig = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
      rm -rf "/home/${config.home.username}/.config/discordcanary/settings.json"
      mkdir -p "/home/${config.home.username}/.config/discordcanary"
    '';

    newDiscordConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
        rm -rf "/home/${config.home.username}/.config/discordcanary/settings.json"
        mkdir -p "/home/${config.home.username}/.config/discordcanary"
        cp ${cfg_file} "/home/${config.home.username}/.config/discordcanary/settings.json"
        chmod 666 "/home/${config.home.username}/.config/discordcanary/settings.json"
    '';
  };

}

# "css": "@import url('https://refact0r.github.io/midnight-discord/midnight.css');/* change colors and variables here */:root {/* amount of spacing and padding */--spacing: 12px;/* radius of round corners */--roundness: 16px;/* color of links */--accent-1: #${colors.base08};/* color of unread dividers and some indicators */--accent-2: #${colors.base0B};/* color of accented buttons */--accent-3: #${colors.base01};/* color of accented buttons when hovered */--accent-4: #${colors.base02};/* color of accented buttons when clicked */--accent-5: #${colors.base08};/* color of mentions and messages that mention you */--mention: #${colors.base00};/* color of mentions and messages that mention you when hovered */--mention-hover: #${colors.base03}60;/* color of bright text on colored buttons */--text-1: #${colors.base0D};/* color of headings and important text */--text-2: #${colors.base0B};/* color of normal text */--text-3: #${colors.base05};/* color of icon buttons and channels */--text-4: #${colors.base05};/* color of muted channels/chats and timestamps */--text-5: #${colors.base0E};/* color of dark buttons when clicked */--bg-1: #${colors.base01}60;/* color of dark buttons */--bg-2: #${colors.base00};/* color of spacing around panels and secondary elements */--bg-3: #${colors.base00};/* main background color */--bg-4: #${colors.base01};/* color of channels and icon buttons when hovered */--hover: #${colors.base00};/* color of channels and icon buttons when clicked or selected */--active: #${colors.base0B}50;/* color of messages when hovered */--message-hover: #${colors.base00}70;}",
