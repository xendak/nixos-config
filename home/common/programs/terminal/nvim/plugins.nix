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
    plugins = [
      # vim-floaterm
      # FTerm-nvim
      pkgs.vimPlugins.vim-nix
      # pkgs.vimPlugins.nvim-tree-lua
      # (pkgs.vimPlugins.nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))
      pkgs.vimPlugins.plenary-nvim
      pkgs.vimPlugins.rainbow-delimiters-nvim
      pkgs.vimPlugins.telescope-fzy-native-nvim
      pkgs.vimPlugins.telescope-nvim
      pkgs.vimPlugins.vim-sneak
      pkgs.vimPlugins.which-key-nvim
      nnn
      {
        plugin = pkgs.vimPlugins.toggleterm-nvim;
        config = ''
          nnoremap <C-/> :ToggleTerm direction=float<cr>
          nnoremap <M-/> :ToggleTerm<cr>
          tnoremap <C-/> <C-\><C-n>:ToggleTerm<cr>
          tnoremap <M-/> <C-\><C-n>:ToggleTerm<cr>

          lua <<EOF
              require('toggleterm').setup()
          EOF
        '';
      }
    ];
  };
}
