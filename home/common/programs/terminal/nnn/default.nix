{ pkgs, ... }:
{
  imports = [
    ./plugins
  ];
  home.packages = with pkgs; [
    (nnn.override { withNerdIcons = true; })
  ];

  home.sessionVariables = {
    NNN_TMPFILE = "$XDG_CONFIG_HOME/nnn/.lastd";
    NNN_FIFO = "/tmp/nnn.fifo";
    NNN_PLUG = "p:preview-tui;l:fzcd;g:rg;f:fd";
  };
}
