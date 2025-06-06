{
  pkgs,
  inputs,
  ...
}:
{
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
    lazygit
    tree

    imhex
    hexyl
    difftastic
    bitwise

    # DEFAULT LANGUAGES i use mostly?
    valgrind
    clang-tools
    llvmPackages_latest.libstdcxxClang
    llvmPackages_latest.libcxx
    llvmPackages_latest.lldb
    cppcheck
    gdb
    inputs.uwu-colors.packages.${pkgs.system}.default

    # language formatters
    # nodePackages.prettier
    # dprint
    # deno

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
    defaultOptions = [ "--color 16" ];
  };

  # enable dir-env integration
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
