{
  pkgs,
  ...
}:
{
  home.packages = [
    pkgs.kakoune-lsp
    pkgs.ripgrep
    pkgs.fd
  ];

  # kak-lsp config lives here
  xdg.configFile."kak/kak-lsp.toml".source = ./kak-lsp.toml;

  programs.kakoune = {
    enable = true;

    plugins = with pkgs.kakounePlugins; [
      prelude-kak # utility functions used by other plugins
      fzf-kak # fzf picker (files, buffers, grep, etc.)
      auto-pairs-kak # bracket/quote auto-pairing
      kakoune-buffers # buffer-list in statusline + navigation
    ];

    config = {
      colorScheme = "default"; # overridden by theme file sourced in extraConfig
      showMatching = true;
      tabStop = 2;
      indentWidth = 2;
      scrollOff = {
        lines = 5;
        columns = 5;
      };
      wrapLines = {
        enable = false; # per-filetype below
        word = true;
        indent = true;
      };
      numberLines = {
        enable = true;
        relative = true;
        highlightCursor = true;
      };
      ui = {
        enableMouse = true;
        assistant = "cat";
      };
      hooks = [
        # ── Filetype indent overrides ──────────────────────────────────────
        {
          name = "WinSetOption";
          option = "filetype=(nix|c|cpp|go|odin)";
          commands = "set-option buffer indentwidth 2; set-option buffer tabstop 2";
        }
        {
          name = "WinSetOption";
          option = "filetype=(py|java|qml|lua|toml)";
          commands = "set-option buffer indentwidth 4; set-option buffer tabstop 4";
        }
        {
          name = "WinSetOption";
          option = "filetype=markdown";
          commands = ''
            set-option buffer autowrap_column 150
            autowrap-enable
            add-highlighter buffer/ wrap -word -indent
          '';
        }

        {
          name = "BufWritePre";
          option = ".*\\.nix";
          commands = "nix-fmt";
        }
        {
          name = "BufWritePre";
          option = ".*\\.(rs)";
          commands = "lsp-formatting-sync";
        }
        {
          name = "BufWritePre";
          option = ".*\\.(zig)";
          commands = "lsp-formatting-sync";
        }
        {
          name = "BufWritePre";
          option = ".*\\.dart";
          commands = "lsp-formatting-sync";
        }
        {
          name = "BufWritePre";
          option = ".*\\.(ts|tsx|js|jsx)";
          commands = "dprint-fmt";
        }
        {
          name = "BufWritePre";
          option = ".*\\.md";
          commands = "deno-fmt-md";
        }
        {
          name = "BufWritePre";
          option = ".*\\.py";
          commands = "black-fmt";
        }
        {
          name = "BufWritePre";
          option = ".*\\.(sh|bash)";
          commands = "shfmt-fmt";
        }
        {
          name = "BufWritePre";
          option = ".*\\.fish";
          commands = "fish-indent-fmt";
        }
        {
          name = "BufWritePre";
          option = ".*\\.(css|scss|html|yaml)";
          commands = "prettier-fmt";
        }
        {
          name = "BufWritePre";
          option = ".*\\.go";
          commands = "gofmt-fmt";
        }
        {
          name = "BufWritePre";
          option = ".*\\.(cpp|cc|cxx)";
          commands = "clang-fmt-google";
        }

        # ── kak-lsp: attach LSP to known filetypes ─────────────────────────
        {
          name = "WinSetOption";
          option = "filetype=(rust|go|nix|python|c|cpp|zig|odin|dart|java|lua|bash|fish|javascript|typescript|tsx|jsx|markdown|qml)";
          commands = ''
            lsp-enable-window
            lsp-inlay-hints-enable window
            map window lsp <ret> ': lsp-hover<ret>' -docstring "LSP hover"
          '';
        }

        # ── fzf-kak: set options after modules load ──────────────────────────
        {
          name = "ModuleLoaded";
          option = "fzf";
          commands = ''
            set-option global fzf_highlight_command '${pkgs.bat}/bin/bat --color=always --style=plain --theme=base16'
            set-option global fzf_use_main_selection false
          '';
        }
        {
          name = "ModuleLoaded";
          option = "fzf-file";
          commands = "set-option global fzf_file_command '${pkgs.fd}/bin/fd --type f --follow --hidden --exclude .git/ --strip-cwd-prefix --color never'";
        }
        {
          name = "ModuleLoaded";
          option = "fzf-grep";
          commands = "set-option global fzf_grep_command '${pkgs.ripgrep}/bin/rg --column --with-filename --line-number --color=never'";
        }

        # ── kakoune-buffers ────────────────────────────────────────────────
        {
          name = "ModuleLoaded";
          option = "kakoune-buffers";
          commands = ''
            hook global WinDisplay .* info-buffers
          '';
        }
      ];

      keyMappings = [
        {
          mode = "normal";
          key = "'\\'";
          effect = "<a-|>";
          docstring = "Pipe selection into";
        }
        # ── File picker (yazi, like helix A-e / space-e) ──────────────────
        {
          mode = "normal";
          key = "<a-e>";
          effect = ": yazi-pick<ret>";
          docstring = "Open yazi at current file";
        }
        {
          mode = "normal";
          key = "<a-g>";
          effect = ": lazygit-open<ret>";
          docstring = "Open lazygit";
        }

        # ── Paragraph motions preserved ───────────────────────────────────
        # Kakoune uses { } natively for paragraph — no remapping needed.

        # ── Tab: move to end/start of tree-sitter node (best approximation)
        # Kakoune doesn't have parent node movement; use ] [ for next/prev object
        {
          mode = "normal";
          key = "<tab>";
          effect = "]p";
          docstring = "Next paragraph";
        }
        {
          mode = "normal";
          key = "<s-tab>";
          effect = "[p";
          docstring = "Prev paragraph";
        }

        # ── space leader ──────────────────────────────────────────────────
        {
          mode = "user";
          key = "<space>";
          effect = ": enter-user-mode session<ret>";
          docstring = "Session submenu";
        }
        {
          mode = "user";
          key = "C";
          effect = ": delete-buffer<ret>";
          docstring = "Close buffer";
        }
        {
          mode = "user";
          key = "n";
          effect = ": buffer-next<ret>";
          docstring = "Next buffer";
        }
        {
          mode = "user";
          key = "p";
          effect = ": buffer-previous<ret>";
          docstring = "Prev buffer";
        }
        # space-e: yazi at current file
        {
          mode = "user";
          key = "e";
          effect = ": yazi-pick<ret>";
          docstring = "Yazi (current file)";
        }
        # space-E: yazi at git root
        {
          mode = "user";
          key = "E";
          effect = ": yazi-pick-root<ret>";
          docstring = "Yazi (git root)";
        }
        # space-g: lazygit
        {
          mode = "user";
          key = "g";
          effect = ": lazygit-open<ret>";
          docstring = "Lazygit";
        }
        # space-i: toggle inlay hints
        {
          mode = "user";
          key = "i";
          effect = ": lsp-inlay-hints-toggle window<ret>";
          docstring = "Toggle inlay hints";
        }
        # space-f: fzf file
        {
          mode = "user";
          key = "f";
          effect = ": fzf-mode<ret>";
          docstring = "fzf picker";
        }

        # ── Compilation mode (THE reason you're here) ─────────────────────
        # space-m: :make submenu
        {
          mode = "user";
          key = "m";
          effect = ": enter-user-mode make<ret>";
          docstring = "Make/build submenu";
        }

        # ── Search (rg/fd) ────────────────────────────────────────────────
        {
          mode = "user";
          key = "/";
          effect = ": fzf-mode<ret>g";
          docstring = "Grep (rg via fzf)";
        }
        {
          mode = "user";
          key = "r";
          effect = ": grep-next-match<ret>";
          docstring = "Next grep match";
        }
        {
          mode = "user";
          key = "R";
          effect = ": grep-previous-match<ret>";
          docstring = "Prev grep match";
        }

        # ── LSP bindings under space-l ─────────────────────────────────────
        {
          mode = "user";
          key = "l";
          effect = ": enter-user-mode lsp<ret>";
          docstring = "LSP submenu";
        }
      ];
    };

    extraConfig = ''
      # ── Source theme (generated by your color system) ────────────────────
      # Put your generated theme at ~/.config/kak/colors/current.kak
      # The helix.nix template will need a parallel kak template.
      colorscheme current

      # ── Whitespace rendering ─────────────────────────────────────────────
      add-highlighter global/ show-whitespaces -tab '⇥' -lf '↴' -spc ' '

      # add-highlighter global/ column 80 default,rgb:222222
      # add-highlighter global/ line '%val{cursor_line}' default+b
      set-face global CursorLine default,default+b


      # session user-mode (space-space-*)
      declare-user-mode session 
      map global session q ': quit!<ret>' -docstring "Quit"
      map global session s ': write<ret>' -docstring "Save"

      # :COMPILATION
      define-command -override terminal -params 1.. -docstring "open in zellij split" %{
        nop %sh{
          zellij action new-pane --floating --name Float --close-on-exit -- "$@"
        }
      }
      # Default make command (override per project via .kakrc or :set)
      set-option global makecmd 'make'

      define-command my-make -docstring "make + show *make* in Command pane" %{
        declare-option -hidden str my_prev_buffer
        set-option global my_prev_buffer %val{bufname}
        make
        buffer %opt{my_prev_buffer}
        nop %sh{
          /home/xendak/Flake/bin/kak-zellij.sh "$kak_session" &
        }
      }

      # Rust / Cargo — parse rustc JSON for file:line:col
      define-command cargo-build -docstring "cargo build (make buffer)" %{
        set-option buffer makecmd 'cargo build 2>&1'
        my-make
      }
      define-command cargo-check -docstring "cargo check (fast)" %{
        set-option buffer makecmd 'cargo check 2>&1'
        my-make
      }
      define-command cargo-test -docstring "cargo test" %{
        set-option buffer makecmd 'cargo test 2>&1'
        my-make
      }
      define-command cargo-clippy -docstring "cargo clippy" %{
        set-option buffer makecmd 'cargo clippy 2>&1'
        my-make
      }
      define-command cargo-run -docstring "cargo run" %{
        set-option buffer makecmd 'cargo run 2>&1'
        my-make
      }

      # Go
      define-command go-build -docstring "go build ./..." %{
        set-option buffer makecmd 'go build ./... 2>&1'
        my-make
      }
      define-command go-test -docstring "go test ./..." %{
        set-option buffer makecmd 'go test ./... 2>&1'
        my-make
      }
      define-command go-vet -docstring "go vet ./..." %{
        set-option buffer makecmd 'go vet ./... 2>&1'
        my-make
      }

      # Zig
      define-command zig-build -docstring "zig build" %{
        set-option buffer makecmd 'zig build 2>&1'
        my-make
      }
      define-command zig-test -docstring "zig build test" %{
        set-option buffer makecmd 'zig build test 2>&1'
        my-make
      }
      define-command zig-run -docstring "zig build run" %{
        set-option buffer makecmd 'zig build run 2>&1'
        my-make
      }

      # C / C++ (cmake / ninja / make)
      define-command cmake-build -docstring "cmake --build build" %{
        set-option buffer makecmd 'cmake --build build 2>&1'
        my-make
      }
      define-command ninja-build -docstring "ninja -C build" %{
        set-option buffer makecmd 'ninja -C build 2>&1'
        my-make
      }

      # Java (Maven / Gradle)
      define-command mvn-build -docstring "mvn compile" %{
        set-option buffer makecmd 'mvn compile 2>&1'
        my-make
      }
      define-command gradle-build -docstring "gradle build" %{
        set-option buffer makecmd 'gradle build 2>&1'
        my-make
      }

      # Odin
      define-command odin-build -docstring "odin build . -debug" %{
        set-option buffer makecmd 'odin build . -debug 2>&1'
        my-make
      }
      define-command odin-run -docstring "odin run ." %{
        set-option buffer makecmd 'odin run . 2>&1'
        my-make
      }

      # Just (justfile runner)
      define-command just-run -params 0..1 -docstring "just [recipe]" %{
        set-option buffer makecmd %sh{ echo "just ''${1:-} 2>&1" }
        my-make
      }


      define-command cargo-test-single -docstring "cargo test <name>" %{
        prompt "test: " %{
          set-option buffer makecmd %sh{ echo "cargo test $1 2>&1" }
          my-make
        }
      }

      define-command go-test-single -docstring "go test -run <name>" %{
        prompt "test: " %{
          set-option buffer makecmd %sh{ echo "go test ./... -run $1 2>&1" }
          my-make
        }
      }

      define-command zig-test-single -docstring "zig build test --test-filter <name>" %{
        prompt "test: " %{
          set-option buffer makecmd %sh{ echo "zig build test --test-filter $1 2>&1" }
          my-make
        }
      }

      define-command pytest-single -docstring "pytest -k <name>" %{
        prompt "test: " %{
          set-option buffer makecmd %sh{ echo "pytest -k $1 2>&1" }
          my-make
        }
      }

      define-command python-run -docstring "python run current file" %{
        set-option buffer makecmd %sh{ echo "python $kak_buffile 2>&1" }
        my-make
      }

      define-command pytest-run -docstring "pytest all" %{
        set-option buffer makecmd 'pytest 2>&1'
        my-make
      }

      define-command go-run -docstring "go run ." %{
        set-option buffer makecmd 'go run . 2>&1'
        my-make
      }

      define-command mvn-test -docstring "mvn test" %{
        set-option buffer makecmd 'mvn test 2>&1'
        my-make
      }

      # run-cmd: takes the REST of the line as a shell command — no quoting needed.
      # Using -shell-script-candidates gives completion; prompt fills makecmd interactively.
      define-command run-cmd \
        -params 1.. \
        -docstring "run-cmd <cmd…>: run shell command in *make* buffer (no quoting needed)" \
        %{
          set-option buffer makecmd %sh{ printf '%s' "$*" }
          my-make
        }

      # make user-mode (space-m-*)
      hook global ModuleLoaded make %{
        define-command -override -hidden make-open-error -params 4 %{
          evaluate-commands -try-client %opt{jumpclient} %{
            edit -existing "%arg{1}" %arg{2} %arg{3}
            echo -markup "{Information}{\\}%arg{4}"
          }
        }
      }# Lock the first client (your main editor) as the default target for error jumps
      hook global KakBegin .* %{
        try %{
          # If jumpclient isn't set yet, set it to this newly opened client
          evaluate-commands %sh{
            if [ -z "$kak_opt_jumpclient" ]; then
              echo "set-option global jumpclient $kak_client"
            fi
          }
        }
      }
      declare-user-mode make
      define-command make-cmd-smart -docstring "show build commands for current filetype" %{
        evaluate-commands %sh{
          echo "try %{ declare-user-mode make-cmd-ft }"
          case "$kak_opt_filetype" in
            rust)
              echo "
                map window make-cmd-ft b ': cargo-build<ret>'       -docstring 'cargo build'
                map window make-cmd-ft r ': cargo-run<ret>'         -docstring 'cargo run'
                map window make-cmd-ft t ': cargo-test<ret>'        -docstring 'cargo test (all)'
                map window make-cmd-ft z ': cargo-test-single<ret>' -docstring 'cargo test (single)'
                map window make-cmd-ft k ': cargo-check<ret>'       -docstring 'cargo check'
                map window make-cmd-ft K ': cargo-clippy<ret>'      -docstring 'cargo clippy'
                enter-user-mode make-cmd-ft
              "
              ;;
            go)
              echo "
                map window make-cmd-ft b ': go-build<ret>'       -docstring 'go build'
                map window make-cmd-ft r ': go-run<ret>'         -docstring 'go run .'
                map window make-cmd-ft t ': go-test<ret>'        -docstring 'go test (all)'
                map window make-cmd-ft z ': go-test-single<ret>' -docstring 'go test (single)'
                map window make-cmd-ft v ': go-vet<ret>'         -docstring 'go vet'
                enter-user-mode make-cmd-ft
              "
              ;;
            zig)
              echo "
                map window make-cmd-ft b ': zig-build<ret>'       -docstring 'zig build'
                map window make-cmd-ft r ': zig-run<ret>'         -docstring 'zig build run'
                map window make-cmd-ft t ': zig-test<ret>'        -docstring 'zig build test (all)'
                map window make-cmd-ft z ': zig-test-single<ret>' -docstring 'zig test (single)'
                enter-user-mode make-cmd-ft
              "
              ;;
            c|cpp)
              echo "
                map window make-cmd-ft b ': cmake-build<ret>'   -docstring 'cmake build'
                map window make-cmd-ft n ': ninja-build<ret>'   -docstring 'ninja build'
                enter-user-mode make-cmd-ft
              "
              ;;
            python)
              echo "
                map window make-cmd-ft r ': python-run<ret>'    -docstring 'python run'
                map window make-cmd-ft t ': pytest-run<ret>'    -docstring 'pytest (all)'
                map window make-cmd-ft z ': pytest-single<ret>' -docstring 'pytest (single)'
                enter-user-mode make-cmd-ft
              "
              ;;
            java)
              echo "
                map window make-cmd-ft b ': mvn-build<ret>'     -docstring 'mvn compile'
                map window make-cmd-ft t ': mvn-test<ret>'      -docstring 'mvn test'
                map window make-cmd-ft d ': gradle-build<ret>'  -docstring 'gradle build'
                enter-user-mode make-cmd-ft
              "
              ;;
            *)
              echo "
                map window make-cmd-ft j ': just-run<ret>'      -docstring 'just (default)'
                map window make-cmd-ft r ': run-cmd '           -docstring 'run-cmd <shell>'
                enter-user-mode make-cmd-ft
              "
              ;;
          esac
        }
      }

      # Define the smart formatter command
      define-command smart-format -docstring "Format buffer based on extension or filetype with LSP fallback" %{
        evaluate-commands %sh{
          # First evaluate by exact file extension
          case "$kak_buffile" in
            *.css|*.scss|*.html|*.yaml|*.yml)
              echo "prettier-fmt"
              exit 0
              ;;
            *.md)
              echo "deno-fmt-md"
              exit 0
              ;;
            *.ts|*.tsx|*.js|*.jsx)
              echo "dprint-fmt"
              exit 0
              ;;
          esac

          # Then evaluate by Kakoune filetype
          case "$kak_opt_filetype" in
            nix) echo "nix-fmt" ;;
            python) echo "black-fmt" ;;
            sh) echo "shfmt-fmt" ;;
            fish) echo "fish-indent-fmt" ;;
            go) echo "gofmt-fmt" ;;
            c|cpp) echo "clang-fmt-google" ;;
            # Default fallback to standard format-buffer / LSP
            *) echo "format-buffer" ;;
          esac
        }
      }

      # :Commands(Compilation)
      map global make n ': make-next-error<ret>'         -docstring "next error"
      map global make p ': make-previous-error<ret>'     -docstring "prev error"
      map global make m ': my-make<ret>'                 -docstring "re-run last make"
      map global make f ': smart-format<ret>'            -docstring "Smart format"
      map global make c ': make-cmd-smart<ret>'          -docstring "build commands (filetype-aware)"

      # ────────────────────────────────────────────────────────────────────
      # GREP / SEARCH (rg / fd — emacs compilation-mode style)
      #
      # Results land in *grep* buffer, navigable with grep-next-match.
      # ────────────────────────────────────────────────────────────────────
      set-option global grepcmd '${pkgs.ripgrep}/bin/rg --column --with-filename --line-number --color=never'

      define-command rg-word -docstring "rg: search word under cursor in project" %{
        grep -- %val{selection}
      }

      define-command fd-files -params 0..1 -docstring "fd: find files matching pattern" %{
        set-option buffer makecmd %sh{ echo "${pkgs.fd}/bin/fd --type f ''${1:-.}" }
        make
      }

      # ────────────────────────────────────────────────────────────────────
      # YAZI — fifo-based (the only correct approach without terminal-with-output)
      #
      # We write to a temp file, launch yazi with --chooser-file pointing at it,
      # then read the result back.  The 'terminal' command opens in a new window;
      # on zellij/kitty the windowing module provides this.
      # ────────────────────────────────────────────────────────────────────
      define-command yazi-pick \
        -docstring "Open yazi at current file location" \
        %{
          evaluate-commands %sh{
            chooser=$(mktemp /tmp/kak-yazi-XXXXXX)
            # 'terminal' is kakoune's windowing-agnostic command
            printf '%s\n' "terminal sh -c '${pkgs.yazi}/bin/yazi \"\$1\" --chooser-file=\"\$2\"; printf done > \"\$2\".done' -- %val{buffile} $chooser"
            printf '%s\n' "
              hook -once global FocusIn .* %{
                evaluate-commands %sh{
                  # wait briefly for yazi to write its result
                  i=0
                  while [ \$i -lt 50 ] && [ ! -f '${"\${chooser}"}.done' ]; do
                    sleep 0.1; i=\$((i+1))
                  done
                  if [ -s '$chooser' ]; then
                    file=\$(cat '$chooser')
                    rm -f '$chooser' '${"\${chooser}"}.done'
                    printf 'edit -- \"%s\"\n' \"\$file\"
                  else
                    rm -f '$chooser' '${"\${chooser}"}.done'
                  fi
                }
              }
            "
          }
        }

      define-command yazi-pick-root \
        -docstring "Open yazi at project root" \
        %{
          evaluate-commands %sh{
            chooser=$(mktemp /tmp/kak-yazi-XXXXXX)
            root=$(git rev-parse --show-toplevel 2>/dev/null || echo .)
            printf '%s\n' "terminal sh -c '${pkgs.yazi}/bin/yazi \"\$1\" --chooser-file=\"\$2\"; printf done > \"\$2\".done' -- $root $chooser"
            printf '%s\n' "
              hook -once global FocusIn .* %{
                evaluate-commands %sh{
                  i=0
                  while [ \$i -lt 50 ] && [ ! -f '${"\${chooser}"}.done' ]; do
                    sleep 0.1; i=\$((i+1))
                  done
                  if [ -s '$chooser' ]; then
                    file=\$(cat '$chooser')
                    rm -f '$chooser' '${"\${chooser}"}.done'
                    printf 'edit -- \"%s\"\n' \"\$file\"
                  else
                    rm -f '$chooser' '${"\${chooser}"}.done'
                  fi
                }
              }
            "
          }
        }


      # ────────────────────────────────────────────────────────────────────
      # LAZYGIT integration (mirrors helix A-g / space-g)
      # ────────────────────────────────────────────────────────────────────
      define-command lazygit-open -docstring "Open lazygit in a new terminal" %{
        terminal ${pkgs.lazygit}/bin/lazygit
      }

      # ────────────────────────────────────────────────────────────────────
      # kak-lsp bootstrap
      # ────────────────────────────────────────────────────────────────────
      eval %sh{ ${pkgs.kakoune-lsp}/bin/kak-lsp --kakoune -s $kak_session }
    '';
  };
}
