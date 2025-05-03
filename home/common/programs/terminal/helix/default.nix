{
  lib,
  pkgs,
  config,
  # inputs,
  ...
}: let
  inherit (config) colorscheme;
in {
  home.sessionVariables.COLORTERM = "truecolor";
  programs.helix = {
    enable = true;
    # package = inputs.helix.packages.${pkgs.system}.default;
    # package = inputs.helix.packages.${pkgs.system}.default.overrideAttrs (old: {
    #   makeWrapperArgs = with pkgs;
    #     old.makeWrapperArgs
    #     or []
    #     ++ [
    #       "--suffix"
    #       "PATH"
    #       ":"
    #       (lib.makeBinPath [
    #         clang-tools
    #         marksman
    #         nil
    #         efm-langserver
    #         nodePackages.bash-language-server
    #         nodePackages.vscode-langservers-extracted
    #         vscode-extensions.llvm-org.lldb-vscode
    #         nodePackages.prettier
    #         shellcheck
    #       ])
    #     ];
    # });

    settings = {
      theme = colorscheme.slug;
      editor = {
        color-modes = true;
        line-number = "relative";
        cursorline = true;

        end-of-line-diagnostics = "hint";
        lsp = {
          enable = true;
          auto-signature-help = true;
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
            ":open %sh{cat /tmp/unique-file}"
            ":redraw"
          ];
          "A-g" = [
            ":new"
            ":insert-output ${pkgs.lazygit}/bin/lazygit"
            ":buffer-close!"
            ":redraw"
          ];
        };

        normal.space = {
          i = ":toggle lsp.display-inlay-hints";
          S-c = ":buffer-close";
          n = "goto_next_buffer";
          S-n = "goto_previous_buffer";
        };

        normal.space.space = {
          s = ":write";
          q = ":quit";
        };
        normal.space.u = {
          f = ":format"; # format using LSP formatter
          w = ":set whitespace.render all";
          W = ":set whitespace.render none";
        };
      };
    };

    themes = import ./theme.nix {inherit colorscheme;};
    languages = import ./languages.nix {inherit lib pkgs config;};
  };
}
