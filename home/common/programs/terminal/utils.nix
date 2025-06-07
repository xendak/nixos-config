{
  pkgs,
  inputs,
  ...
}:
let

  theme-switcher = (
    pkgs.writeShellScriptBin "theme-switcher" ''
      HELIX_THEME_DIR="$HOME/.config/helix/themes"
      WEZTERM_THEME_DIR="$HOME/.config/wezterm/colors"

      case $1 in
        "dark")
          NEW_FILE="dark"
        ;;
        "light")
          NEW_FILE="light"
        ;;
        *)
          NEW_FILE="default"
        ;;
      esac

      cat "$HELIX_THEME_DIR/$NEW_FILE.toml" >"$HELIX_THEME_DIR/current.toml"
      sed -i "s/##/#/g" "$HELIX_THEME_DIR/current.toml"

      cat "$WEZTERM_THEME_DIR/$NEW_FILE.lua" >"$WEZTERM_THEME_DIR/current.lua"
      sed -i "s/##/#/g" "$WEZTERM_THEME_DIR/current.lua"

      echo "Theme switched to $NEW_FILE."
    ''
  );
in
{
  home.packages = with pkgs; [
    theme-switcher

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
