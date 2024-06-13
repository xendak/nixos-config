{pkgs, ...}: {
  imports = [
    ./kitty.nix
    ./fish.nix
    ./zoxide.nix
    ./git.nix
    ./starship.nix
    ./nvim
    ./nnn
    ./helix
    ./bitwarden.nix
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
    unzip
    unrar
    p7zip
    xdg-utils

    bat

    clang-tools

    nixd # Nix LSP
    alejandra
    nixfmt-rfc-style # Nix formatter
    nvd # Differ
    nix-output-monitor
    nh # Nice wrapper for NixOS and HM

    ltex-ls # Spell checking LSP
  ];
  programs.fzf = {
    enable = true;
    defaultOptions = ["--color 16"];
  };
}
