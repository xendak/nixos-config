{ lib, paletteSet, ... }:
# @import url('https://refact0r.github.io/system24/build/system24.css');
# .content_f75fb0 {
#   gap: var(--gap);
# }

# .interactive_bf202d {
#   color: var(--interactive-text-default) !important;
# }

# .title_c38106 {
# 	opacity: 0 !important;
# 	display: none;
# }

# .c38106a3f0c3ca76-title {
#   display: none;
# }

# .c38106a3f0c3ca76-winButton {
#   width: 32px !important;
#   height: 32px !important;
# }

# .trailing_c38106 {
#   top: calc(var(--top-bar-height) + var(--border-thickness));
#   right: calc(var(--gap) + var(--border-thickness));
#   gap: var(--space-xs);
#   z-index: 1000;
#   height: 32px;
#   position: relative !important;
# }
let
  p = paletteSet.palette;
  rawCss =
    # css
    ''
      @import url('https://refact0r.github.io/system24/build/system24.css');
      body {
      	--font: 'Sofia Pro';

        --gap: 12px;
        --divider-thickness: 4px;

        --animations: on;
        --list-item-transition: 0.2s ease;
        --dms-icon-svg-transition: 0.4s ease;

        --move-top-bar-buttons: on;
        --custom-app-top-bar-height: 12px;

        --custom-window-controls: false;
        --window-control-size: 18px;

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
        --text-1: ${p.on_surface};
        --text-2: ${p.on_surface};
        --text-3: ${p.on_surface};
        --text-4: ${p.dim};
        --text-5: ${p.comments};

        --bg-1: ${p.surface_container};
        --bg-2: ${p.surface_container_high};
        --bg-3: ${p.surface_container_low};
        --bg-4: ${p.bg};
        --hover: ${p.primary}20;
        --active: ${p.primary}60;
        --message-hover: ${p.background};

        --accent-1: ${p.secondary};
        --accent-2: ${p.tertiary};
        --accent-3: ${p.primary};
        --accent-4: ${p.blue};
        --accent-5: ${p.green};
        --accent-new: var(--accent-2);

        --online: ${p.green};
        --dnd: ${p.yellow};
        --idle: ${p.yellow};
        --streaming: ${p.blue};
        --offline: ${p.on_surface_variant};

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

# {
#   lib,
#   config,
#   paletteSet,
#   ...
# }:
# let
#   p = paletteSet.palette;
#   f = config.fontProfiles;

#   colorUtils = import ../palettes/colors.nix { inherit lib; };
#   accentHsl = colorUtils.hexToHsl p.primary;
#   alertHsl = colorUtils.hexToHsl p.error;
#   warningHsl = colorUtils.hexToHsl p.yellow;

#   rawCss =
#     # css
#     ''
#       /* Import Material Discord and the Material You Addon */
#       @import url("https://capnkitten.github.io/BetterDiscord/Themes/Material-Discord/css/source.css");
#       @import url("https://capnkitten.github.io/BetterDiscord/Themes/Material-Discord/css/addons/material-you/source.css");

#       .theme-dark {
#         --saturation-modifier: 0.31;
#         --lightness-modifier: 0.225;
#         --text-lightness-modifier: 1;
#         --ui-darkness-modifier: 1;
#       }

#       .theme-light {
#         --saturation-modifier: 0.74;
#         --lightness-modifier: 2.125;
#         --text-lightness-modifier: 1;
#       }

#       :root {
#         --app-font: "${f.regular.family}";
#         --app-font-width: 100;
#         --app-font-roundness: 100;

#         --app-bg: ${p.bg};
#         --main-color: ${p.surface_container};
#         --main-alt: ${p.surface_container_low};
#         --server-color: transparent;
#         --status-picker-color: ${p.surface_container_high};
#         --sidebar-panel-color: ${p.surface_container_high};
#         --chat-color: ${p.bg};
#         --avatar-radius: 24px;

#         --accent-hue: ${toString accentHsl.h};
#         --accent-saturation: ${toString accentHsl.s}%;
#         --accent-lightness: ${toString accentHsl.l}%;
#         --accent-text-color: ${p.on_primary};
#         --accent-button-action: ${p.primary};

#         --alert-hue: ${toString alertHsl.h};
#         --alert-saturation: ${toString alertHsl.s}%;
#         --alert-lightness: ${toString alertHsl.l}%;
#         --alert-text-color: ${p.on_error};

#         --warning-hue: ${toString warningHsl.h};
#         --warning-saturation: ${toString warningHsl.s}%;
#         --warning-lightness: ${toString warningHsl.l}%;
#         --warning-text-color: ${p.on_surface};

#         --message-color: ${p.surface_container};
#         --message-color-hover: ${p.surface_container_high};
#         --message-color-alt: ${p.surface_container_highest};
#         --message-radius: 18px;
#         --message-padding-top: 8px;
#         --message-padding-side: 12px;

#         --media-radius: 10px;
#         --attachment-color: ${p.surface_variant};

#         --main-textarea-color: ${p.surface_container_high};
#         --main-textarea-color-alt: ${p.surface_container_highest};
#         --main-textarea-border: ${p.outline_variant};
#         --main-textarea-radius: 24px;

#         --input-color: ${p.surface_variant};
#         --input-color-alt: ${p.surface_container_high};
#         --input-border-color: ${p.outline};
#         --input-text-color: ${p.on_surface};
#         --input-placeholder-color: ${p.on_surface_variant};
#         --input-height: 40px;
#         --input-padding: 12px;

#         --popout-color: ${p.surface_container};
#         --popout-color-alt: ${p.surface_container_high};
#         --popout-header-border: ${p.outline_variant};
#         --separator-color: ${p.outline_variant};
#         --popout-radius: 8px;
#         --popout-radius-big: 18px;

#         --card-color-filled: ${p.surface_variant};
#         --card-color-hover: ${p.surface_container_highest};
#         --card-color-active: ${p.surface_container_high};
#         --card-color-alt: ${p.surface};
#         --card-color-alt-hover: ${p.surface_container};
#         --card-border-color: ${p.outline_variant};
#         --card-header-text-color: ${p.on_surface};
#         --card-radius: 8px;
#         --card-radius-big: 18px;

#         --button-height: 40px;
#         --button-padding: 0 24px;
#         --button-radius: 18px;
#         --button-link-color: transparent;
#         --button-link-text-color: ${p.on_surface};

#         --switch-knob-color: ${p.outline};
#         --switch-slider-color: ${p.surface_variant};

#         --tooltip-color: ${p.inverse_surface};
#         --tooltip-text-color: ${p.inverse_on_surface};
#         --tooltip-font-size: 12px;
#         --tooltip-padding: 8px;
#         --tooltip-radius: 8px;

#         --scrollbar-color: ${p.surface_variant};
#         --scrollbar-width: 10px;
#         --scrollbar-thin-width: 6px;
#       }
#     '';

#   escapedCss = lib.concatStringsSep "\n" (lib.splitString "\n" rawCss);

#   jsonStructure = {
#     SKIP_HOST_UPDATE = true;
#     openasar = {
#       setup = true;
#       quickstart = true;
#       css = escapedCss;
#     };
#     BACKGROUND_COLOR = "${p.bg}";
#     offloadAdmControls = false;
#     enableHardwareAcceleration = true;
#     chromiumSwitches = { };
#     MINIMIZE_TO_TRAY = false;
#     OPEN_ON_STARTUP = false;
#   };
# in
# {
#   "vesktop/quickCss.css" = rawCss;
#   "discord/settings.json" = builtins.toJSON jsonStructure;
# }
