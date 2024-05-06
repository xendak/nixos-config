{ pkgs, ... }:
let
  nnn = pkgs.vimUtils.buildVimPlugin {
    name = "nnn";
    src = pkgs.fetchFromGitHub {
      owner = "mcchrish";
      repo = "nnn.vim";
      rev = "e0104e369508fc12e3651ad4dee20261b5b3e87f";
      hash = "sha256-ZLokeEA70pdVmKOjK5vB8tRE0zGHicsunIHgW1Px0sw=";
    };
  };
in
{
  programs.neovim = {
    plugins = with pkgs.vimPlugins; [
      vim-nix
      nvim-tree-lua
      (nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))
      plenary-nvim
      rainbow-delimiters-nvim
      telescope-fzy-native-nvim
      telescope-nvim
      vim-floaterm
      vim-sneak
      which-key-nvim
      nnn
    ];
  };
}
