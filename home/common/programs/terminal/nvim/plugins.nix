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
  compile-mode = pkgs.vimUtils.buildVimPlugin {
    name = "compile-mode";
    src = pkgs.fetchFromGitHub {
      owner = "ej-shafran";
      repo = "compile-mode.nvim";
      rev = "6b41499bd782be2c213011072ce0f0eb9f7b78a2";
      hash = "sha256-AoEuE+BLQwAHgvkanLUU6kd4HhAyn9Y53lRAYnoghz4=";
    };
    dependencies = [
      pkgs.vimPlugins.plenary-nvim
    ];
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
      compile-mode
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
