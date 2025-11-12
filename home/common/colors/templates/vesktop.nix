{ lib, paletteSet, ... }:
let
  p = paletteSet.palette;
  rawCss =
    # css
    ''
      @import url('https://refact0r.github.io/midnight-discord/build/midnight.css');
      body {
      	--font: 'Sofia Pro';

        --gap: 12px;
        --divider-thickness: 4px;

        --animations: on;
        --list-item-transition: 0.2s ease;
        --dms-icon-svg-transition: 0.4s ease;

        --move-top-bar-buttons: on;
        --custom-app-top-bar-height: 12px;

        --custom-window-controls: on;
        --window-control-size: 36px;

        --dms-icon: on;
        --dms-icon-svg-size: 90%;
        --dms-icon-color-before: var(--icon-secondary);
        --dms-icon-color-after: var(--white);

        --dms-background: off;
        --dms-background-image-url: url("");
        --dms-background-image-size: cover;
        --dms-background-color: linear-gradient(70deg, var(--blue-2), var(--purple-2), var(--red-2));

        --background-image: off;
        --background-image-url: url("");

        --transparency-tweaks: off;
        --remove-bg-layer: off;
        --panel-blur: off;
        --blur-amount: 12px;
        --bg-floating: var(--bg-3);

        --small-user-panel: off;
      }

      :root {
        --text-0: var(--bg-4);
        --text-1: ${p.base04};
        --text-2: ${p.base05};
        --text-3: ${p.base05};
        --text-4: ${p.base04};
        --text-5: ${p.base03};

        --bg-1: ${p.base01};
        --bg-2: ${p.base02};
        --bg-3: ${p.base01};
        --bg-4: ${p.bg};
        --hover: ${p.accent}20;
        --active: ${p.accent}60;
        --message-hover: ${p.base00};

        --accent-1: ${p.base0D};
        --accent-2: ${p.base0D};
        --accent-3: ${p.accent};
        --accent-4: ${p.base0C};
        --accent-5: ${p.base0B};
        --accent-new: var(--accent-2);

        --online: ${p.base0B};
        --dnd: ${p.base08};
        --idle: ${p.base0A};
        --streaming: ${p.base0E};
        --offline: var(--red-2);

        --mention: linear-gradient(to right, color-mix(in hsl, var(--blue-2), transparent 90%) 40%, transparent);
        --mention-hover: linear-gradient(to right, color-mix(in hsl, var(--blue-2), transparent 95%) 40%, transparent);
        --reply: linear-gradient(to right, color-mix(in hsl, var(--text-3), transparent 90%) 40%, transparent);
        --reply-hover: linear-gradient(to right, color-mix(in hsl, var(--text-3), transparent 95%) 40%, transparent);

        --border-light: hsl(230, 20%, 40%, 0.1);
        --border: hsl(230, 20%, 40%, 0.2);
        --button-border: hsl(0, 0%, 100%, 0.1);

        --red-1: oklch(76% 0.12 0);
        --red-2: oklch(70% 0.12 0);
        --red-3: oklch(64% 0.12 0);
        --red-4: oklch(58% 0.12 0);
        --red-5: oklch(52% 0.12 0);

        --green-1: oklch(76% 0.11 170);
        --green-2: oklch(70% 0.11 170);
        --green-3: oklch(64% 0.11 170);
        --green-4: oklch(58% 0.11 170);
        --green-5: oklch(52% 0.11 160);

      	--blue-11: oklch(from hsl(216, 45%, 69%) calc(l -0.1) c h);
      	--blue-12: oklch(from hsl(313, 100%, 50%) calc(l -0.1) c h);
      	--blue-13: oklch(from hsl(52, 12%, 64%) calc(l -0.1) c h);
      	--blue-14: oklch(from hsl(4, 15%, 62%) calc(l -0.1) c h);
      	--blue-15: oklch(from hsl(216, 45%, 69%) calc(l -0.1) c h);

      	--blue-1: oklch(0.7 0.1 248);
        --blue-2: oklch(0.92 0.06 75);
        --blue-3: oklch(0.85 0.05 75);
        --blue-4: oklch(0.87 0.06 75);
        --blue-5: oklch(0.63 0.23 340); 

        --yellow-1: oklch(80% 0.11 90);
        --yellow-2: oklch(74% 0.11 90);
        --yellow-3: oklch(68% 0.11 90);
        --yellow-4: oklch(62% 0.11 90);
        --yellow-5: oklch(56% 0.11 90);

        --purple-1: oklch(76% 0.11 310);
        --purple-2: oklch(70% 0.11 310);
        --purple-3: oklch(64% 0.11 310);
        --purple-4: oklch(58% 0.11 310);
        --purple-5: oklch(52% 0.11 310);
      }

      .content_f75fb0 {
        gap: var(--gap);
      }

      .title_c38106 {
      	opacity: 0 !important;
      }

      .trailing_c38106 {
        position: relative;
        vertical-align: center;
        top: calc(
          var(--custom-app-top-bar-height) + -2px + var(--custom-guild-list-padding)
        ) !important;
      }

    '';
  escapedCss = lib.concatStringsSep "\n" (lib.splitString "\n" rawCss);

  jsonStructure = {
    SKIP_HOST_UPDATE = true;
    openasar = {
      setup = true;
      quickstart = true;
      css = escapedCss;
    };
    BACKGROUND_COLOR = "${p.bg}";
    offloadAdmControls = false;
    enableHardwareAcceleration = true;
    chromiumSwitches = { };
    MINIMIZE_TO_TRAY = false;
    OPEN_ON_STARTUP = false;
  };
in
{
  "vesktop/quickCss.css" = rawCss;
  "discord/settings.json" = builtins.toJSON jsonStructure;
}
