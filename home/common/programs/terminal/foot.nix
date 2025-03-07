{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config.colorscheme) colors;
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
        foreground = "${colors.base05}";
        background = "${colors.base00}";
        regular0 = "${colors.base00}";   # black
        regular1 = "${colors.base08}";   # red
        regular2 = "${colors.base0B}";   # green
        regular3 = "${colors.base0A}";   # yellow
        regular4 = "${colors.base0D}";   # blue
        regular5 = "${colors.base0E}";   # magenta
        regular6 = "${colors.base0C}";   # cyan
        regular7 = "${colors.base05}";    # white
        bright0 = "${colors.base03}";     # bright black
        bright1 = "${colors.base08}";    # bright red
        bright2 = "${colors.base0B}";    # bright green
        bright3 = "${colors.base0A}";     # bright yellow
        bright4 = "${colors.base0D}";     # bright blue
        bright5 = "${colors.base0E}";     # bright magenta
        bright6 = "${colors.base0C}";     # bright cyan
        bright7 = "${colors.base07}";     # bright white
        selection-foreground = "${colors.base00}";
        selection-background = "${colors.base05}";
        urls = "${colors.base04}";
      };
    };
  };
}
