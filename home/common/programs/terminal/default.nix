{ pkgs, ...}:
{
  imports = [
    ./kitty.nix 
    ./fish.nix 
    ./git.nix
    ./nvim
    ./nnn
    ./starship.nix
    # ./nix-index.nix
  ];
  home.packages = with pkgs; [
    bc # Calculator
    bottom # System viewer
    ncdu # TUI disk usage
    eza # Better ls
    ripgrep # Better grep
    fd # Better find
    httpie # Better curl
    diffsitter # Better diff
    jq # JSON pretty printer and manipulator

    nil # Nix LSP
    nixfmt # Nix formatter
    nvd # Differ
    nix-output-monitor
    nh # Nice wrapper for NixOS and HM

    ltex-ls # Spell checking LSP
  ];
  programs.fzf = {
    enable = true;
    defaultOptions = [ "--color 16" ];
  };
}
