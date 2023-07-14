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
    "css": "\n\n/* IMPORTANT: make sure to enable dark mode in discord settings for the theme to apply properly!! */\n\n@import url('https://refact0r.github.io/midnight-discord/midnight.css');\n\n/* change colors and variables here */\n:root {\n\t/* amount of spacing and padding */\n\t--spacing: 12px;\n\t/* radius of round corners */\n\t--roundness: 16px;\n\n\t/* color of links */\n\t--accent-1: #${colors.base08};\n\t/* color of unread dividers and some indicators */\n\t--accent-2: #${colors.base05};\n\t/* color of accented buttons */\n\t--accent-3: #${colors.base01};\n\t/* color of accented buttons when hovered */\n\t--accent-4: #${colors.base01}90;\n\t/* color of accented buttons when clicked */\n\t--accent-5: #${colors.base02}90;\n\n\t/* color of mentions and messages that mention you */\n\t--mention: #${colors.base03};\n\t/* color of mentions and messages that mention you when hovered */\n\t--mention-hover: #${colors.base03}60;\n\n\t/* color of bright text on colored buttons */\n\t--text-1: #${colors.base0D};\n\t/* color of headings and important text */\n\t--text-2: #${colors.base0B};\n\t/* color of normal text */\n\t--text-3: #${colors.base05};\n\t/* color of icon buttons and channels */\n\t--text-4: #${colors.base0C}90;\n\t/* color of muted channels/chats and timestamps */\n\t--text-5: #${colors.base04}60;\n\n\t/* color of dark buttons when clicked */\n\t--bg-1: #${colors.base01}60;\n\t/* color of dark buttons */\n\t--bg-2: #${colors.base00};\n\t/* color of spacing around panels and secondary elements */\n\t--bg-3: #${colors.base00}80;\n\t/* main background color */\n\t--bg-4: #${colors.base00};\n\n\t/* color of channels and icon buttons when hovered */\n\t--hover: #${colors.base0C};\n\t/* color of channels and icon buttons when clicked or selected */\n\t--active: #${colors.base01}70;\n\t/* color of messages when hovered */\n\t--message-hover: #${colors.base00}70;\n}",
    "quickstart": true
  }
}
  ''; 
}
