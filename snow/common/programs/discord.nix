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
        "css": "@import url('https://refact0r.github.io/midnight-discord/midnight.css');\n\n:root {\n\t/* link color */\n\t--link-color: #${colors.base08};\n    /* main accent color (mentions, folder icons, unread indicators) */\n\t--accent-color: #${colors.base0C};\n\t/* darker accent color (background of any accent items with text) */\n\t--accent-background: #${colors.base00};\n\t/* even darker accent color (background of accent buttons when hover) */\n\t--accent-background-2: #${colors.base01};\n\t/* even darker accent color (background of accent buttons when clicked) */\n\t--accent-background-3: #${colors.base02}90;\n\t/* background of mention text and messages that mention you */\n\t--mention-modifier: #${colors.base03};\n\t/* background of messages that mention you on hover */\n\t--mention-hover-modifier: #${colors.base03}60;\n\t/* bright text for colored buttons */\n\t--bright-text: #${colors.base0D};\n\t/* header text for headers and certain text */\n\t--header-text: #${colors.base0B};\n\t/* main text color (messages, titles, unread text/indicators) */\n\t--primary-text: #${colors.base05};\n\t/* secondary text color (channels, descriptions, buttons) */\n\t--secondary-text: #${colors.base04};\n\t/* muted text color (input box placeholders, muted channels, message times) */\n\t--muted-text: #${colors.base04}60;\n\t/* background of muted colored buttons on hover */\n\t--muted-background-hover: #${colors.base03}30;\n\t/* background of muted colored buttons on click */\n\t--muted-background-active: #${colors.base03}90;\n\t/* background of pretty much everything not using primary background */\n\t--secondary-background: #${colors.base01};\n\t/* main chat/channel list/member list background */\n\t--primary-background: #${colors.base00};\n\t/* overlay background on stuff when hovered over */\n\t--hover-modifier: #${colors.base03};\n\t/* overlay background on stuff when clicked or selected */\n\t--active-modifier: #${colors.base01};\n\t/* overlay background on messages when hovered over */\n\t--message-hover-modifier: #${colors.base03}90;\n\t--status-danger: #${colors.base08};\n\t--roundness: 16px;\n}\n",
        "quickstart": true
      },
      "OPEN_ON_STARTUP": false
    }
  ''; 
}
