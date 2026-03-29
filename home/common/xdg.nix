{ config, pkgs, ... }:
let
  browser = [ "zen.desktop" ];
  terminal = [ "foot.desktop" ];

  # XDG MIME types
  associations = {
    "application/x-extension-htm" = browser;
    "application/x-extension-html" = browser;
    "application/x-extension-shtml" = browser;
    "application/x-extension-xht" = browser;
    "application/x-extension-xhtml" = browser;
    "application/xhtml+xml" = browser;
    "text/html" = browser;
    "x-scheme-handler/about" = browser;
    "x-scheme-handler/chrome" = browser;
    "x-scheme-handler/ftp" = browser;
    "x-scheme-handler/http" = browser;
    "x-scheme-handler/https" = browser;
    "x-scheme-handler/unknown" = browser;
    "x-scheme-handler/terminal" = terminal;

    "audio/*" = [ "mpv.desktop" ];
    "video/*" = [ "mpv.dekstop" ];
    "image/*" = [ "imv-dir.desktop" ];
    "image/jpeg" = [ "imv-dir.desktop" ];
    "image/png" = [ "imv-dir.desktop" ];
    "application/json" = browser;
    "application/pdf" = [ "org.pwmt.zathura.desktop.desktop" ];
    "x-scheme-handler/discord" = [ "discord-canary.desktop" ];
  };
in
{

  home.packages = [
    pkgs.handlr-regex
    pkgs.walker
    (pkgs.writeShellScriptBin "xdg-open" ''handlr open "$@"'')
    (pkgs.writeShellScriptBin "xterm" "handlr launch x-scheme-handler/terminal -- \"$@\"")
    # (pkgs.lib.hiPrio (pkgs.writeShellScriptBin "xdg-open" ''handlr open "$@"''))
    # (pkgs.lib.hiPrio (
    #   pkgs.writeShellScriptBin "xterm" "handlr launch x-scheme-handler/terminal -- \"$@\""
    # ))
  ];
  xdg = {
    enable = true;
    cacheHome = config.home.homeDirectory + "/.local/cache";

    mimeApps = {
      enable = true;
      associations.added = associations;
      defaultApplications = associations;
    };

    # TODO: maybe not? eventually replace with qs or ags
    configFile = {
      "handlr/handlr.toml".text = ''
        enable_selector = true
        selector = "walker -d -k -p 'Open with:'"
        term_exec_args = '-e'
        expand_wildcards = true
      '';
    };

    userDirs = {
      setSessionVariables = true;
      enable = true;
      createDirectories = true;
      extraConfig = {
        SCREENSHOTS = "${config.xdg.userDirs.pictures}/Screenshots";
      };
    };
  };
}
