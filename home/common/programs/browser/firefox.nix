{ config, pkgs, lib, inputs, ... }:
let
	addons = inputs.firefox-addons.packages.${pkgs.system};
in
{
	programs.browserpass.enable = true;
	programs.firefox = {
		enable = true;
		profiles.Snow = {
			bookmarks = {};
			extensions = with addons; [
				ublock-origin
        vimium
        violentmonkey
        bitwarden
        browserpass
        stylus
        add-custom-search-engine
			];
			settings = {
				"browser.disableResetPrompt" = true;
				"browser.download.panel.shown" = true;
				"browser.download.useDownloadDir" = false;
				"browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
				"browser.shell.checkDefaultBrowser" = false;
				"browser.shell.defaultBrowserCheckCount" = 1;
				"browser.uiCustomization.state" = ''{"placements":{"widget-overflow-fixed-list":[],"nav-bar":["back-button","forward-button","stop-reload-button","home-button","zoom-controls", "urlbar-container","downloads-button","library-button","ublock0_raymondhill_net-browser-action","_testpilot-containers-browser-action"],"toolbar-menubar":["menubar-items"],"TabsToolbar":["tabbrowser-tabs","new-tab-button","alltabs-button"],"PersonalToolbar":["import-button","personal-bookmarks"]},"seen":["save-to-pocket-button","developer-button","ublock0_raymondhill_net-browser-action","_testpilot-containers-browser-action"],"dirtyAreaCache":["nav-bar","PersonalToolbar","toolbar-menubar","TabsToolbar","widget-overflow-fixed-list"],"currentVersion":18,"newElementCount":4}'';
				"dom.security.https_only_mode" = true;
				"identity.fxaccounts.enabled" = false;
				"privacy.trackingprotection.enabled" = true;
				"signon.rememberSignons" = true;
				"app.update.auto" = false;
				"fission.autostart" = true;
				"media.ffmpeg.vaapi.enabled" = true;
				"media.rdd-ffmpeg.enabled" = true;
			};

			# Search Engines
			search.engines = {
				"Nix Packages" = {
					urls = [{
						template = "https://search.nixos.org/packages";
						params = [
						{ name = "channel"; value = "unstable"; }
						{ name = "type"; value = "packages"; }
						{ name = "query"; value = "{searchTerms}"; }
						];
					}];

					icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
					definedAliases = [ "@np" ];
				};

				"Nix Options" = {
					urls = [{
						template = "https://search.nixos.org/options";
						params = [
						{ name = "channel"; value = "unstable"; }
						{ name = "type"; value = "packages"; }
						{ name = "query"; value = "{searchTerms}"; }
						];
					}];

					icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
					definedAliases = [ "@no" ];
				};

				"Home-manager Options" = {
					urls = [{
						template = "https://mipmip.github.io/home-manager-option-search/?{searchTerms}";
					}];

					icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
					definedAliases = [ "@hm" ];
				};
			};
		};
	};

	home = {
		sessionVariables = {
			BROWSER = "firefox";
			DEFAULT_BROWSER = "firefox";
		};
		persistence = {
                  "/persist/home/${config.home.username}" = {
                    directories = [ ".mozilla/firefox" ];
		    allowOther = true; 
                  };
		};
	};

	xdg.mimeApps.defaultApplications = {
		"text/html" = [ "firefox.desktop" ];
		"text/xml" = [ "firefox.desktop" ];
		"x-scheme-handler/http" = [ "firefox.desktop" ];
		"x-scheme-handler/https" = [ "firefox.desktop" ];
		"x-scheme-handler/about" = [ "firefox.desktop" ];
		"x-scheme-handler/unknown" = [ "firefox.desktop" ];
	};

}
