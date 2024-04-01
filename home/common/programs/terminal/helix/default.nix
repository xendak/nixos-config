{ config, ... }:
let
  inherit (config) colorscheme;
in
{
  home.sessionVariables.COLORTERM = "truecolor";
  programs.helix = {
    enable = true;
    settings = {
      theme = colorscheme.slug;
      editor = {
        color-modes = true;
        line-number = "relative";
        cursorline = true;
        lsp.display-inlay-hints = true;
        indent-guides.render = true;
        cursor-shape = {
          normal = "block";
          insert = "bar";
          select = "underline";
        };
      };
    };
    themes = import ./theme.nix { inherit colorscheme; };
    languages = import ./languages.nix { inherit config; };
  };
}
