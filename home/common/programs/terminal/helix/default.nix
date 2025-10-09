{
  lib,
  pkgs,
  config,
  inputs,
  ...
}:
{
  home.sessionVariables.COLORTERM = "truecolor";
  programs.helix = {
    enable = true;
    package = pkgs.helix;
    # package = inputs.helix-flake.packages.${pkgs.system}.default;
    extraPackages = [
      # pkgs.marksman
      pkgs.markdown-oxide
      # pkgs.dprint
      # pkgs.deno
      # pkgs.nil
      pkgs.shellcheck
      pkgs.clang-tools
      pkgs.nodePackages.vscode-langservers-extracted
      pkgs.vscode-extensions.llvm-org.lldb-vscode
      # pkgs.nodePackages.prettier
    ];

    settings = {
      # theme = darkTheme.colorscheme.slug;
      # set default as it was for WSL
      theme =
        if config.home.username == "nixos" then "${config.themes.default.colorScheme.slug}" else "current";

      editor = {
        color-modes = true;
        line-number = "relative";
        cursorline = true;
        # i dont understand this, sometimes is needed and sometimes is not.
        # mouse = true;

        end-of-line-diagnostics = "hint";
        lsp = {
          enable = true;
          auto-signature-help = false;
          display-inlay-hints = true;
          display-progress-messages = true;
          display-signature-help-docs = true;
          display-messages = true;
          goto-reference-include-declaration = true;
          snippets = true;
        };

        inline-diagnostics = {
          cursor-line = "hint";
        };

        indent-guides.render = true;
        smart-tab = {
          enable = false;
        };
        cursor-shape = {
          normal = "block";
          insert = "bar";
          select = "underline";
        };

        whitespace.characters = {
          newline = "↴";
          tab = "⇥";
        };
      };
      keys = {
        insert = {
          "C-[" = "goto_prev_paragraph";
          "C-]" = "goto_next_paragraph";
          "C-a" = "signature_help";
          "C-p" = ":toggle lsp.display-inlay-hints";
        };

        select = {
          "{" = "goto_prev_paragraph";
          "}" = "goto_next_paragraph";
        };

        normal = {
          X = "extend_line_above";
          "{" = "goto_prev_paragraph";
          "}" = "goto_next_paragraph";
          tab = "move_parent_node_end";
          S-tab = "move_parent_node_start";
          "A-e" = [
            ":sh rm -f /tmp/unique-file"
            ":insert-output ${pkgs.yazi}/bin/yazi %{buffer_name} --chooser-file=/tmp/unique-file"
            ":insert-output echo \"\x1b[?1049h\x1b[?2004h\" > /dev/tty"
            ":set mouse false"
            ":set mouse true"
            ":open %sh{cat /tmp/unique-file}"
            ":redraw"
          ];
          "A-g" = [
            ":new"
            ":insert-output ${pkgs.lazygit}/bin/lazygit"
            ":set mouse false"
            ":set mouse true"
            ":buffer-close!"
            ":redraw"
          ];
        };

        normal.space = {
          i = ":toggle lsp.display-inlay-hints";
          S-c = ":buffer-close";
          n = "goto_next_buffer";
          S-n = "goto_previous_buffer";
          S-e = [
            ":sh rm -f /tmp/unique-file"
            ":insert-output ${pkgs.yazi}/bin/yazi %sh{git rev-parse --show-toplevel} --chooser-file=/tmp/unique-file"
            ":insert-output echo \"\x1b[?1049h\x1b[?2004h\" > /dev/tty"
            ":open %sh{cat /tmp/unique-file}"
            ":set mouse false"
            ":set mouse true"
            ":redraw"
          ];
          e = [
            ":sh rm -f /tmp/unique-file"
            ":insert-output ${pkgs.yazi}/bin/yazi %{buffer_name} --chooser-file=/tmp/unique-file"
            ":insert-output echo \"\x1b[?1049h\x1b[?2004h\" > /dev/tty"
            ":open %sh{cat /tmp/unique-file}"
            ":set mouse false"
            ":set mouse true"
            ":redraw"
          ];
          g = [
            ":new"
            ":insert-output ${pkgs.lazygit}/bin/lazygit"
            ":buffer-close!"
            ":set mouse false"
            ":set mouse true"
            ":redraw"
          ];
        };

        normal.space.space = {
          s = ":write";
          q = ":quit!";
        };
        normal.space.u = {
          f = ":format"; # format using LSP formatter
          w = ":set whitespace.render all";
          W = ":set whitespace.render none";
        };
      };
    };

    # themes = import ./theme.nix { inherit colorscheme; };
    languages = import ./languages.nix {
      inherit
        lib
        pkgs
        config
        inputs
        ;
    };
  };
}
