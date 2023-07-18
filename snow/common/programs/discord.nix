{ config, pkgs, lib, ... }:

let inherit (config.colorscheme) colors;
in {
  home.packages = with pkgs; [ 
    (discord-canary.override { 
      nss = nss_latest; 
      withOpenASAR = true;
    })
  ];

  home.persistence = {
    "/persist/snow/flakes".directories = [ ".config/discordcanary" ];
    "/persist/snow/flakes".allowOther = true;
  };

  xdg.configFile."discordcanary/settings.json".text = ''
{
  "SKIP_HOST_UPDATE": true,
  "openasar": {
    "setup": true,
    "css": "@import url('https://refact0r.github.io/midnight-discord/midnight.css');/* change colors and variables here */:root {/* amount of spacing and padding */--spacing: 12px;/* radius of round corners */--roundness: 16px;/* color of links */--accent-1: #${colors.base08};/* color of unread dividers and some indicators */--accent-2: #${colors.base0B};/* color of accented buttons */--accent-3: #${colors.base01};/* color of accented buttons when hovered */--accent-4: #${colors.base02};/* color of accented buttons when clicked */--accent-5: #${colors.base02}90;/* color of mentions and messages that mention you */--mention: #${colors.base00};/* color of mentions and messages that mention you when hovered */--mention-hover: #${colors.base03}60;/* color of bright text on colored buttons */--text-1: #${colors.base0D};/* color of headings and important text */--text-2: #${colors.base0B};/* color of normal text */--text-3: #${colors.base05};/* color of icon buttons and channels */--text-4: #${colors.base0C}90;/* color of muted channels/chats and timestamps */--text-5: #${colors.base04}60;/* color of dark buttons when clicked */--bg-1: #${colors.base01}60;/* color of dark buttons */--bg-2: #${colors.base00};/* color of spacing around panels and secondary elements */--bg-3: #${colors.base00}80;/* main background color */--bg-4: #${colors.base01};/* color of channels and icon buttons when hovered */--hover: #${colors.base00};/* color of channels and icon buttons when clicked or selected */--active: #${colors.base01}70;/* color of messages when hovered */--message-hover: #${colors.base00}70;}",
    "quickstart": true
  }
}
  ''; 
}
