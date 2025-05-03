{
  config,
  pkgs,
  ...
}: let
  inherit (config.colorscheme) palette;
in {
  xdg.configFile."vesktop/settings/quickCss.css".source =
    pkgs.writeText "quickCss.css"
    ''
      @import url('https://refact0r.github.io/midnight-discord/midnight.css');
      :root {
        --spacing: 12px;
        --roundness: 16px;
        --font: 'Sofia Pro';
        --font-code: monospace;
        --online-indicator: var(--text-2);
        --moon-icon: block;
        --discord-icon: none;
        --accent-1: #${palette.base0D};
        --accent-2: #${palette.base08};
        --accent-3: #${palette.base0B}60;
        --accent-4: #${palette.base02};
        --accent-5: #${palette.base00};
        --mention: #${palette.base01}80;
        --mention-hover: #${palette.base01}c0;
        --text-1: #${palette.base07};
        --text-2: #${palette.base0B};
        --text-3: #${palette.base05};
        --text-4: #${palette.base05}c0;
        --text-5: #${palette.base03};
        --bg-1: #${palette.base02};
        --bg-2: #${palette.base0C}90;
        --bg-3:  #${palette.base01};
        --bg-4: #${palette.base00};
        --hover: #${palette.base0C}20;
        --active: #${palette.base0C}60;
        --message-hover: #${palette.base05}20;
        --text-0: var(--text-3)
        --list-item-transition: 0.2s ease;
        --unread-bar-transition: 0.2s ease;
        --moon-spin-transition: 0.4s ease;
        --icon-spin-transition: 1s ease;
        --roundness-xl: 22px;
        --roundness-l: 20px;
        --roundness-m: 16px;
        --roundness-s: 12px;
        --roundness-xs: 10px;
        --roundness-xxs: 8px;
        --discord-icon: none;
        --moon-icon: block;
        --moon-icon-url: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 27 27' width='24' height='24'%3E%3Cpath fill='currentColor' d='M0 0h7v1H6v1H5v1H4v1H3v1H2v1h5v1H0V6h1V5h1V4h1V3h1V2h1V1H0m13 2h5v1h-1v1h-1v1h-1v1h3v1h-5V7h1V6h1V5h1V4h-3m8 5h1v5h1v-1h1v1h-1v1h1v-1h1v1h-1v3h-1v1h-2v1h-1v1h1v-1h2v-1h1v2h-1v1h-2v1h-1v-1h-1v1h-6v-1h-1v-1h-1v-2h1v1h2v1h3v1h1v-1h-1v-1h-3v-1h-4v-4h1v-2h1v-1h1v-1h1v2h1v1h1v-1h1v1h-1v1h2v-2h1v-2h1v-1h1M8 14h2v1H9v4h1v2h1v1h1v1h1v1h4v1h-6v-1H5v-1H4v-5h1v-1h1v-2h2m17 3h1v3h-1v1h-1v1h-1v2h-2v-2h2v-1h1v-1h1m1 0h1v3h-1v1h-2v-1h1v-1h1'%3E%3C/path%3E%3C/svg%3E");
        --moon-icon-size: cover;
        --login-bg-filter: none;
        --green-to-accent-3-filter: none;
        --blurple-to-accent-3-filter: none;
      }
      .wrapper__8436d:hover > .childWrapper_a6ce15::before,
      .wrapper__8436d.selected_ae80f7 > .childWrapper_a6ce15::before {
          background: var(--text-0);
          transform: rotate(-0deg) scale(0.8);
      }
    '';
}
