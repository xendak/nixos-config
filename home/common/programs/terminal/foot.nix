{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config.colorscheme) palette;
  # xterm = {
  #   foot = pkgs.writeShellScriptBin "xterm" ''
  #     ${pkgs.foot}/bin/foot "$@"
  #   '';
  #   # terminfo = pkgs.runCommand "terminfo" {} ''
  #   #   ${pkgs.ncurses}/bin/tic -x -o $out ${pkgs.foot}/etc/foot/terminfo
  #   # '';
  # };
in {
  # optional
  # home = {
  #   packages = [
  #     xterm.foot
  #   ];
  #   sessionVariables = {
  #     TERMINAL = lib.mkForce "foot";
  #   };
  #   sessionPath = ["$HOME/Flake/bin"];
  # };

  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "${config.fontProfiles.monospace.family}:size=12";
        pad = "15x15";
        term = "xterm-256color";
      };
      colors = {
        foreground = "${palette.base05}";
        background = "${palette.base00}";
        regular0 = "${palette.base00}"; # black
        regular1 = "${palette.base08}"; # red
        regular2 = "${palette.base0B}"; # green
        regular3 = "${palette.base0A}"; # yellow
        regular4 = "${palette.base0D}"; # blue
        regular5 = "${palette.base0E}"; # magenta
        regular6 = "${palette.base0C}"; # cyan
        regular7 = "${palette.base05}"; # white
        bright0 = "${palette.base03}"; # bright black
        bright1 = "${palette.base08}"; # bright red
        bright2 = "${palette.base0B}"; # bright green
        bright3 = "${palette.base0A}"; # bright yellow
        bright4 = "${palette.base0D}"; # bright blue
        bright5 = "${palette.base0E}"; # bright magenta
        bright6 = "${palette.base0C}"; # bright cyan
        bright7 = "${palette.base07}"; # bright white
        selection-foreground = "${palette.base00}";
        selection-background = "${palette.base05}";
        urls = "${palette.base04}";
      };
    };
  };
}
