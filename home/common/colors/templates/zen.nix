{ paletteSet, ... }:
let
  p = paletteSet.palette;

  scheme = if paletteSet.type == "dark" then "dark" else "light";
in
{
  "zen/userChrome.css" =
    # css
    ''
      @media (prefers-color-scheme: ${scheme}) {
        :root {
          --zen-colors-primary: ${p.surface_container} !important;
          --zen-primary-color: ${p.primary} !important;
          --zen-colors-secondary: ${p.surface_container} !important;
          --zen-colors-tertiary: ${p.surface_container_low} !important;
          --zen-colors-border: ${p.primary} !important;
          --toolbarbutton-icon-fill: ${p.primary} !important;
          --lwt-text-color: ${p.fg} !important;
          --toolbar-field-color: ${p.fg} !important;
          --tab-selected-textcolor: ${p.primary} !important;
          --toolbar-field-focus-color: ${p.fg} !important;
          --toolbar-color: ${p.fg} !important;
          --newtab-text-primary-color: ${p.fg} !important;
          --arrowpanel-color: ${p.fg} !important;
          --arrowpanel-background: ${p.bg} !important;
          --sidebar-text-color: ${p.fg} !important;
          --lwt-sidebar-text-color: ${p.fg} !important;
          --lwt-sidebar-background-color: ${p.surface_container_low} !important;
          --toolbar-bgcolor: ${p.surface_container} !important;
          --newtab-background-color: ${p.bg} !important;
          --zen-themed-toolbar-bg: ${p.surface_container_low} !important;
          --zen-main-browser-background: ${p.surface_container_low} !important;
          --toolbox-bgcolor-inactive: ${p.surface_container_low} !important;
        }

        .zen-view-splitter-header {
          background-color: ${p.surface_container_low} !important;
          color: ${p.primary} !important;
        }
        #tabbrowser-tabpanels[zen-split-view="true"]
          .browserSidebarContainer.deck-selected {
          &:not(.zen-glance-overlay) {
            outline: 1px solid ${p.primary} !important;
          }
        }

        #permissions-granted-icon {
          color: ${p.surface_container_low} !important;
        }

        .sidebar-placesTree {
          background-color: ${p.bg} !important;
        }

        #zen-workspaces-button {
          background-color: ${p.bg} !important;
        }

        #TabsToolbar {
          background-color: ${p.surface_container_low} !important;
        }

        .urlbar-background {
          background-color: ${p.bg} !important;
        }

        .content-shortcuts {
          background-color: ${p.bg} !important;
          border-color: ${p.primary} !important;
        }

        .urlbarView-url {
          color: ${p.primary} !important;
        }

        #zenEditBookmarkPanelFaviconContainer {
          background: ${p.surface_container_low} !important;
        }

        #zen-media-controls-toolbar {
          & #zen-media-progress-bar {
            &::-moz-range-track {
              background: ${p.surface_container} !important;
            }
          }
        }

        toolbar .toolbarbutton-1 {
          &:not([disabled]) {
            &:is([open], [checked])
              > :is(
                .toolbarbutton-icon,
                .toolbarbutton-text,
                .toolbarbutton-badge-stack
              ) {
              fill: ${p.surface_container_low};
            }
          }
        }

        .identity-color-blue {
          --identity-tab-color: ${p.blue} !important;
          --identity-icon-color: ${p.blue} !important;
        }

        .identity-color-turquoise {
          --identity-tab-color: ${p.cyan} !important;
          --identity-icon-color: ${p.cyan} !important;
        }

        .identity-color-green {
          --identity-tab-color: ${p.green} !important;
          --identity-icon-color: ${p.green} !important;
        }

        .identity-color-yellow {
          --identity-tab-color: ${p.yellow} !important;
          --identity-icon-color: ${p.yellow} !important;
        }

        .identity-color-orange {
          --identity-tab-color: ${p.orange} !important;
          --identity-icon-color: ${p.orange} !important;
        }

        .identity-color-red {
          --identity-tab-color: ${p.red} !important;
          --identity-icon-color: ${p.red} !important;
        }

        .identity-color-pink {
          --identity-tab-color: ${p.magenta} !important;
          --identity-icon-color: ${p.magenta} !important;
        }

        .identity-color-purple {
          --identity-tab-color: ${p.magenta} !important;
          --identity-icon-color: ${p.magenta} !important;
        }

        hbox#titlebar {
          background-color: ${p.surface_container_low} !important;
        }

        #zen-appcontent-navbar-container {
          background-color: ${p.surface_container_low} !important;
        }

        /* ── Fix Zen Twilight acrylic/blur bleeding through wrapper gaps ── */
        #zen-main-app-wrapper,
        #zen-appcontent-wrapper,
        #zen-sidebar-splitter,
        #zen-browser-background {
          appearance: unset !important;
          -moz-default-appearance: none !important;
        }

        #zen-main-app-wrapper,
        #zen-appcontent-wrapper,
        #zen-sidebar-splitter,
        #zen-browser-background,
        .zen-browser-generic-background,
        .zen-toolbar-background,
        #navigator-toolbox,
        #tabbrowser-tabbox,
        #main-window {
          background: ${p.surface_container_low} !important;
          background-color: ${p.surface_container_low} !important;
        }

        #sidebar-splitter {
          border: none !important;
          background: ${p.surface_container_low} !important;
        }

        #zen-sidebar-splitter {
          border: none !important;
          background: ${p.surface_container_low} !important;
          background-color: ${p.surface_container_low} !important;
          opacity: 1 !important;
        }

        #zen-sidebar-splitter:hover {
          background: ${p.surface_container} !important;
          background-color: ${p.surface_container} !important;
        }

        @media (-moz-platform: macos) {
          :root {
            --zen-themed-toolbar-bg-transparent: ${p.surface_container_low} !important;
            --zen-main-browser-background-toolbar: ${p.surface_container_low} !important;
            --zen-main-browser-background-old: ${p.surface_container_low} !important;
            --zen-main-browser-background-toolbar-old: ${p.surface_container_low} !important;
            --zen-navigator-toolbox-background: ${p.surface_container_low} !important;
          }
        }
      }
    '';

  "zen/userContent.css" =
    #css
    ''
            @media (prefers-color-scheme: dark) {
        @-moz-document url-prefix("about:") {
          :root {
            --in-content-page-color: ${p.fg} !important;
            --color-accent-primary: ${p.primary} !important;
            --color-accent-primary-hover: ${p.primary_container} !important;
            --color-accent-primary-active: ${p.primary_container} !important;
            background-color: ${p.bg} !important;
            --in-content-page-background: ${p.bg} !important;
          }
        }

        @-moz-document url("about:newtab"), url("about:home") {
          :root {
            --newtab-background-color: ${p.bg} !important;
            --newtab-background-color-secondary: ${p.surface_container} !important;
            --newtab-element-hover-color: ${p.surface_container} !important;
            --newtab-text-primary-color: ${p.fg} !important;
            --newtab-wordmark-color: ${p.fg} !important;
            --newtab-primary-action-background: ${p.primary} !important;
          }

          .icon {
            color: ${p.primary} !important;
          }

          .search-wrapper .logo-and-wordmark .logo {
            background:
              url("zen-logo-mocha.svg"),
              url("https://raw.githubusercontent.com/IAmJafeth/zen-browser/main/themes/Mocha/Rosewater/zen-logo-mocha.svg")
                no-repeat center !important;
            display: inline-block !important;
            height: 82px !important;
            width: 82px !important;
            background-size: 82px !important;
          }

          @media (max-width: 609px) {
            .search-wrapper .logo-and-wordmark .logo {
              background-size: 64px !important;
              height: 64px !important;
              width: 64px !important;
            }
          }

          .card-outer:is(:hover, :focus, .active):not(.placeholder) .card-title {
            color: ${p.primary} !important;
          }

          .top-site-outer .search-topsite {
            background-color: ${p.blue} !important;
          }

          .compact-cards .card-outer .card-context .card-context-icon.icon-download {
            fill: ${p.green} !important;
          }
        }

        @-moz-document url-prefix("about:preferences") {
          :root {
            --zen-colors-tertiary: ${p.surface_container_low} !important;
            --in-content-text-color: ${p.fg} !important;
            --link-color: ${p.primary} !important;
            --link-color-hover: ${p.primary_container} !important;
            --zen-colors-primary: ${p.surface_container} !important;
            --in-content-box-background: ${p.surface_container} !important;
            --zen-primary-color: ${p.primary} !important;
          }

          groupbox,
          moz-card {
            background: ${p.bg} !important;
          }

          button,
          groupbox menulist {
            background: ${p.surface_container} !important;
            color: ${p.fg} !important;
          }

          .main-content {
            background-color: ${p.surface_container_low} !important;
          }

          .identity-color-blue      { --identity-tab-color: ${p.blue} !important;    --identity-icon-color: ${p.blue} !important; }
          .identity-color-turquoise { --identity-tab-color: ${p.cyan} !important;    --identity-icon-color: ${p.cyan} !important; }
          .identity-color-green     { --identity-tab-color: ${p.green} !important;   --identity-icon-color: ${p.green} !important; }
          .identity-color-yellow    { --identity-tab-color: ${p.yellow} !important;  --identity-icon-color: ${p.yellow} !important; }
          .identity-color-orange    { --identity-tab-color: ${p.orange} !important;  --identity-icon-color: ${p.orange} !important; }
          .identity-color-red       { --identity-tab-color: ${p.red} !important;     --identity-icon-color: ${p.red} !important; }
          .identity-color-pink      { --identity-tab-color: ${p.magenta} !important; --identity-icon-color: ${p.magenta} !important; }
          .identity-color-purple    { --identity-tab-color: ${p.magenta} !important; --identity-icon-color: ${p.magenta} !important; }
        }

        @-moz-document url-prefix("about:addons") {
          :root {
            --zen-dark-color-mix-base: ${p.surface_container_low} !important;
            --background-color-box: ${p.bg} !important;
          }
        }

        @-moz-document url-prefix("about:protections") {
          :root {
            --zen-primary-color: ${p.bg} !important;
            --social-color: ${p.magenta} !important;
            --coockie-color: ${p.cyan} !important;
            --fingerprinter-color: ${p.yellow} !important;
            --cryptominer-color: ${p.blue} !important;
            --tracker-color: ${p.green} !important;
            --in-content-primary-button-background-hover: ${p.primary_container} !important;
            --in-content-primary-button-text-color-hover: ${p.fg} !important;
            --in-content-primary-button-background: ${p.surface_container_high} !important;
            --in-content-primary-button-text-color: ${p.fg} !important;
          }

          .card {
            background-color: ${p.surface_container} !important;
          }
        }
      }
    '';
}
